{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

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

main :: IO ()
main =
  do messages <- atomically $ newTChan
     action <- atomically $ newEmptyTMVar
     aWLC <- async (runWLC messages action)
     hSetBuffering stdout NoBuffering
     aReflex <- async (host messages action reactivand)
     (_,exOra) <-
       waitAnyCatchCancel [aWLC,aReflex]
     putStrLn (show exOra)


runWLC :: TChan (DSum Tag) -> TMVar (IO ()) -> IO ()
runWLC messages action = do
  keyboardKeyW <- wrapKey (kKey messages action)
  viewCreatedW <- wrapCreated (vCreated messages action)
  viewFocusW <- wrapFocus vFocus
  _ <- wlcInit (def & keyboard . keyboardKey .~ keyboardKeyW
                    & view . viewCreated .~ viewCreatedW
                    & view . viewFocus .~ viewFocusW) []
  wlcRun
  wlcTerminate
  freeHaskellFunPtr keyboardKeyW
  freeHaskellFunPtr viewCreatedW

data Identifier = IKey | IView deriving (Eq,Ord)

data Tag a where
     TKey :: Tag Key
     TViewCreated :: Tag ViewCreated

instance GEq Tag where
  geq TKey TKey = Just Refl
  geq TKey TViewCreated = Nothing
  geq TViewCreated TKey = Nothing
  geq TViewCreated TViewCreated = Just Refl

instance DMap.GCompare (Tag) where
  gcompare TKey TKey = GEQ
  gcompare TKey TViewCreated = GLT
  gcompare TViewCreated TKey = GGT
  gcompare TViewCreated TViewCreated = GEQ

data Key =
  Key WLCKeyState
      Keysym
      (Set WLCModifier)
  deriving (Show,Eq)

data ViewCreated =
  ViewCreated WLCHandle
  deriving (Show,Eq,Ord)

-- data InMessage = Key WLCKeyState Keysym (Set WLCModifier) | ViewCreated WLCHandle deriving Show

kKey :: TChan (DSum Tag) -> TMVar (IO ()) -> WLCHandle -> CUInt -> WLCModifiersPtr -> CUInt -> CUInt -> WLCKeyStateBit -> IO CBool
kKey messages action _view _ modifiersPtr _ symBit keyStateBit =
  do mods <- getModifiers <$> modifiers
     atomically $
       writeTChan messages
                  (TKey :=> Key (getKeyState keyStateBit) sym mods)
     act <- atomically $ takeTMVar action
     act
     return 1
  where sym = getSym symBit
        modifiers = peek modifiersPtr

vCreated :: TChan (DSum Tag) -> TMVar (IO ()) -> WLCHandle -> IO CBool
vCreated messages action view =
  do atomically $
       writeTChan messages
                  (TViewCreated :=> ViewCreated view)
     act <- atomically $ takeTMVar action
     act
     return 1

vFocus :: WLCHandle -> CBool -> IO ()
vFocus view focus =
  do wlcViewSetState view
                     WlcBitActivated
                     (focus /= 0)

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

reactiveKey :: (Reflex t,MonadHold t m,MonadFix m) => Event t Key -> m (Event t (IO ()))
reactiveKey =
  return .
  fmapMaybe (\case
               Key WlcKeyStatePressed sym mods ->
                 if mods ==
                    fromList [WlcBitModAlt] &&
                    sym == keysym_Return
                    then Just (void $
                               spawnCommand "weston-terminal")
                    else Nothing
               _ -> Nothing)

reactiveViewCreated :: (Reflex t, MonadHold t m, MonadFix m) => Event t ViewCreated -> m (Event t (IO ()))
reactiveViewCreated = return . fmapMaybe (\(ViewCreated view) -> Just (wlcViewFocus view))

reactivand :: WindowManager t m
reactivand e =
  do let selector = fan (DMap.fromList . (:[]) <$> e) -- create singleton maps
     keyEv <- reactiveKey (select selector TKey)
     viewCreatedEv <- reactiveViewCreated (select selector TViewCreated)
     return $ mergeWith (>>) [keyEv,viewCreatedEv]

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
