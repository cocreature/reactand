{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens hiding (view)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Default
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Sum
import           Data.GADT.Compare
import           Data.IORef
import           Data.Set hiding (filter)
import           Foreign hiding (void)
import           Foreign.C.Types
import           Reflex
import           Reflex.Host.Class
import           System.IO
import           System.Process
import           Text.XkbCommon
import           Text.XkbCommon.KeysymList
import           WLC
import           WLC.Lenses
import           WLC.Wrapper

import Layout
import StackSet

main :: IO ()
main =
  do messages <- atomically $ newTChan
     action <- atomically $ newEmptyTMVar
     aWLC <- async (runWLC messages action)
     hSetBuffering stdout NoBuffering
     aReflex <- async (host messages action reactand)
     (_,exOra) <-
       waitAnyCatchCancel [aWLC,aReflex]
     putStrLn (show exOra)


runWLC :: TChan (DSum Tag) -> TMVar (IO ()) -> IO ()
runWLC messages action = do
  keyboardKeyW <- wrapKey (kKey messages action)
  viewCreatedW <- wrapCreated (vCreated messages action)
  viewFocusW <- wrapFocus vFocus
  outputCreatedW <- wrapCreated (oCreated messages action)
  _ <- wlcInit (def & keyboard . keyboardKey .~ keyboardKeyW
                    & view . viewCreated .~ viewCreatedW
                    & view . viewFocus .~ viewFocusW
                    & output . outputCreated .~ outputCreatedW) []
  wlcRun
  wlcTerminate
  freeHaskellFunPtr keyboardKeyW
  freeHaskellFunPtr viewCreatedW
  freeHaskellFunPtr viewFocusW

data Identifier = IKey | IView deriving (Eq,Ord)

data Tag a where
     TKey :: Tag Key
     TViewCreated :: Tag ViewCreated
     TOutputCreated :: Tag OutputCreated

instance GEq Tag where
  geq TKey TKey = Just Refl
  geq TViewCreated TViewCreated = Just Refl
  geq TOutputCreated TOutputCreated = Just Refl
  geq _ _ = Nothing

instance DMap.GCompare (Tag) where
  gcompare TKey TKey = GEQ
  gcompare TKey _ = GLT
  gcompare TViewCreated TKey = GGT
  gcompare TViewCreated TViewCreated = GEQ
  gcompare TViewCreated TOutputCreated = GLT
  gcompare TOutputCreated TKey = GGT
  gcompare TOutputCreated TViewCreated = GGT
  gcompare TOutputCreated TOutputCreated = GEQ

data Key =
  Key WLCKeyState
      Keysym
      (Set WLCModifier)
  deriving (Show,Eq)

data ViewCreated =
  ViewCreated WLCHandle WLCHandle
  deriving (Show,Eq,Ord)

data OutputCreated = OutputCreated WLCHandle deriving (Show,Eq,Ord)

emptyStackSet :: StackSet String (DefaultLayout a0) a sid
emptyStackSet =
  StackSet Nothing
           []
           (fmap (\i ->
                    (Workspace (show i)
                               (DefaultLayout)
                               (2 ^ i)
                               Nothing))
                 [0 :: Integer .. 3])

kKey :: TChan (DSum Tag)
     -> TMVar (IO ())
     -> WLCHandle
     -> CUInt
     -> WLCModifiersPtr
     -> CUInt
     -> CUInt
     -> WLCKeyStateBit
     -> IO CBool
kKey messages action _view _ modifiersPtr _ symBit keyStateBit =
  do mods <- getModifiers <$> modifiers
     atomically $
       writeTChan
         messages
         (TKey :=>
          Key (getKeyState keyStateBit) sym mods)
     act <- atomically $ takeTMVar action
     act
     return 1
  where sym = getSym symBit
        modifiers = peek modifiersPtr

vCreated :: TChan (DSum Tag) -> TMVar (IO ()) -> WLCHandle -> IO CBool
vCreated messages action view =
  do output <- wlcViewGetOutput view
     atomically $
       writeTChan
         messages
         (TViewCreated :=>
          ViewCreated view output)
     act <- atomically $ takeTMVar action
     act
     return 1

vFocus :: WLCHandle -> CBool -> IO ()
vFocus view focus =
  do wlcViewSetState view
                     WlcBitActivated
                     (focus /= 0)

oCreated :: TChan (DSum Tag) -> TMVar (IO ()) -> WLCHandle -> IO CBool
oCreated messages action output =
         do atomically $ writeTChan messages (TOutputCreated :=> OutputCreated output)
            act <- atomically $ takeTMVar action
            act
            return 1

getKeyState :: WLCKeyStateBit -> WLCKeyState
getKeyState b = toEnum (fromIntegral b)

getModifiers :: WLCModifiers -> Set WLCModifier
getModifiers (WLCModifiers _ mods) =
  fromList (filter (\modifier ->
                      mods .&.
                      fromIntegral (fromEnum modifier) /=
                      0)
                   (enumFrom WlcBitModShift))

getSym :: CUInt -> Keysym
getSym sym = Keysym (fromIntegral sym)

type WindowManager t m = (Reflex t,MonadHold t m,MonadFix m) => Event t (DSum Tag) -> m (Event t (IO ()))

type StackSetChange i l a sid = StackSet i l a sid -> StackSet i l a sid

-- | react to key events
reactiveKey :: (Reflex t,MonadHold t m,MonadFix m)
            => Event t Key -> m (Event t (StackSetChange i l a sid,IO ()))
reactiveKey =
  return .
  fmapMaybe (\case
               Key WlcKeyStatePressed sym mods ->
                 if mods ==
                    fromList [WlcBitModAlt] &&
                    sym == keysym_Return
                    then Just (id
                              ,void $
                               spawnCommand "weston-terminal")
                    else Nothing
               _ -> Nothing)

-- | react to a newly created view
reactiveViewCreated :: (Reflex t,MonadHold t m,MonadFix m)
                    => Event t ViewCreated
                    -> m (Event t (StackSetChange String (DefaultLayout WLCHandle) WLCHandle CULong,IO ()))
reactiveViewCreated =
  return .
  fmapMaybe (\(ViewCreated view output) ->
               Just (\stackset -> insertViewInOutput stackset view output,wlcViewFocus view))

reactiveOutputCreated :: (Reflex t,MonadHold t m,MonadFix m) => Event t OutputCreated -> m (Event t (StackSetChange i l a WLCHandle, IO ()))
reactiveOutputCreated =
  return . fmapMaybe (\(OutputCreated output) -> Just (createOutput output,return ()))

-- | generate singleton map from dsum.
singleton' :: GCompare k => DSum k -> DMap.DMap k
singleton' = DMap.fromList . (:[])

-- | reactive window manager
reactand :: forall t m. WindowManager t m
reactand e =
  do let selector = fan (singleton' <$> e)
     keyEv <- reactiveKey (select selector TKey)
     viewCreatedEv <-
       reactiveViewCreated (select selector TViewCreated)
     outputCreatedEv <-
       reactiveOutputCreated (select selector TOutputCreated)
     let (stacksetChanges,actions) =
           unzip . fmap splitE $
           [keyEv,viewCreatedEv,outputCreatedEv]
     stacksetChanges' <-
       -- apply accumulated changes to stackset
       mapDyn (\stackSet -> print stackSet >> relayout stackSet) =<<
       (foldDyn ($) emptyStackSet (mergeWith (.) stacksetChanges) :: m (Dynamic t (StackSet String (DefaultLayout WLCHandle) WLCHandle WLCHandle))) 
     return $
       mergeWith (>>) (updated stacksetChanges' : actions)

-- | reflex host for the window manager
host :: TChan (DSum Tag) -> TMVar (IO ()) -> (forall t m. WindowManager t m) -> IO ()
host messages action wm =
  runSpiderHost $
  do (e,eTriggerRef) <- newEventWithTriggerRef
     b <- runHostFrame $ wm e
     handle <- subscribeEvent b -- bennofs said that I should do this
     forever $
       do message <- liftIO $ atomically $ readTChan messages
          mETrigger <- liftIO $ readIORef eTriggerRef
          case mETrigger of
            Nothing -> return () -- nobody cares about our input
            Just eTrigger ->
              (liftIO =<<) $
              fireEventsAndRead [eTrigger :=> message] $ -- pass input
              do r <- readEvent handle -- read the output
                 case r of
                   Nothing ->
                     -- fill tmvar for callbacks
                     return $
                     (atomically $
                      putTMVar action (return ()))
                   Just act ->
                     -- put the action inside a tmvar for execution by callback
                     do (atomically . putTMVar action <$> act)
