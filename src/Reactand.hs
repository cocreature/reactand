{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactand where

import           Control.Lens hiding (view)
import           Control.Monad
import           Control.Monad.Fix
import           Data.Set hiding (map,filter,foldr,split)
import           Reflex
import qualified System.Process as P
import           Text.PrettyPrint.HughesPJClass
import           Text.XkbCommon
import           Text.XkbCommon.KeysymList
import           WLC

import           Helpers
import           Layout
import           LayoutType
import qualified StackSet as S
import qualified Tree as T
import           Types

-- | reactive window manager
reactand :: forall t m. WindowManager t m
reactand e =
  do let selector = fan (singleton' <$> e)
     keyEv <-
       key keyHandlers (select selector TKey)
     viewCreatedEv <-
       viewCreated (select selector TViewCreated)
     viewDestroyedEv <-
       viewDestroyed (select selector TViewDestroyed)
     outputCreatedEv <-
       outputCreated (select selector TOutputCreated)
     outputDestroyedEv <-
       outputDestroyed (select selector TOutputDestroyed)
     outputResolutionEv <-
       outputResolution (select selector TOutputResolution)
     let actions =
           mergeWith (>>) $
           [keyEv
           ,viewCreatedEv
           ,viewDestroyedEv
           ,outputCreatedEv
           ,outputDestroyedEv
           ,outputResolutionEv]
     stackSetDyn <-
       nubDyn <$> foldDyn interpretActions emptyStackSet actions
     stacksetChanges' <-
       -- apply accumulated changes to stackset
       mapDyn
         ((\stackSet ->
             putStrLn (prettyShow stackSet) >>
             relayout stackSet))
         stackSetDyn
     return $
       mergeWith (>>) (updated stacksetChanges' : [interpretIOActions <$> (attach (current stackSetDyn) actions)])

interpretActions :: Actions
                 -> (S.StackSet String WLCViewPtr WLCOutputPtr)
                 -> (S.StackSet String WLCViewPtr WLCOutputPtr)
interpretActions acts stackset =
  foldr (\x acc -> update x . acc) id acts $
  stackset
  where update :: Action
               -> S.StackSet String WLCViewPtr WLCOutputPtr
               -> S.StackSet String WLCViewPtr WLCOutputPtr
        update (CreateOutput output res) =
          S.createOutput output res
        update (InsertView view output) =
          insertViewInOutput horizontalLayout view output
        update (ChangeResolution output new) =
          S.changeResolution output new
        update (DestroyView v) = S.deleteFromStackSet v
        update (DestroyOutput o) = S.removeOutput o
        update (Focus Up) = S.focusUp
        update (Focus Down) = S.focusDown
        update (Swap Down) = S.swapDown
        update (Swap Up) = S.swapUp
        update (Output Up) = S.nextOutput
        update (Output Down) = S.prevOutput
        update Split = S.current . _Just . S.workspace . S.tree %~ T.split
        update (Move Down) = S.current . _Just . S.workspace . S.tree %~
                             T.moveDown
        update (Move Up) = S.current . _Just . S.workspace . S.tree %~ T.moveUp
        update (ViewWorkspace ws) = S.viewWorkspace ws
        update Cycle = S.current . _Just . S.workspace . S.tree . T.focusT .
                       T.layout %~ cycleLayout
        update (MoveViewUp) = S.current . _Just . S.workspace . S.tree %~
                              T.moveViewUp
        update _ = id

interpretIOActions :: (S.StackSet String WLCViewPtr WLCOutputPtr, Actions) -> IO ()
interpretIOActions (s,acts) = mapM_ run acts
  where run :: Action -> IO ()
        run (SpawnCommand c) = void $ P.spawnCommand c
        run (FocusView v) = wlcViewFocus v
        run Close = mapM_ wlcViewClose
                          (s ^? S.current . _Just . S.workspace . S.tree . T.focusT .
                           T.treeElements . _Just . T.focusL . _Left)
        run _ = return ()

-- | react to key events
key :: (Reflex t,MonadHold t m,MonadFix m)
    => [((Set WLCModifier,Keysym),Actions)]
    -> Event t Key
    -> m (Event t Actions)
key handlers =
  return .
  fmapMaybe (\case
               Key WlcKeyStatePressed sym mods ->
                 foldr f Nothing $
                 map snd $
                 filter (\((mods',sym'),_) -> mods == mods' && sym == sym') handlers
               Key _ _ _ -> Nothing)
  where f act Nothing = Just act
        f act (Just act') = Just (act >> act')

keyHandlers ::  [((Set WLCModifier,Keysym),Actions)]
keyHandlers =
  [((fromList [WlcBitModAlt],keysym_Return),return $ SpawnCommand "weston-terminal")
  ,((fromList [WlcBitModAlt],keysym_n),return (Focus Down))
  ,((fromList [WlcBitModAlt],keysym_m),return (Focus Up))
  ,((fromList [WlcBitModAlt,WlcBitModShift],keysym_N),return (Swap Down))
  ,((fromList [WlcBitModAlt,WlcBitModShift],keysym_M),return (Swap Up))
  ,((fromList [WlcBitModAlt],keysym_e),return $ (Output Up))
  ,((fromList [WlcBitModAlt],keysym_a),return $ (Output Down))
  ,((fromList [WlcBitModAlt],keysym_0),return $ ViewWorkspace "0")
  ,((fromList [WlcBitModAlt],keysym_1),return $ ViewWorkspace "1")
  ,((fromList [WlcBitModAlt],keysym_2),return $ ViewWorkspace "2")
  ,((fromList [WlcBitModAlt],keysym_3),return $ ViewWorkspace "3")
  ,((fromList [WlcBitModAlt],keysym_4),return $ ViewWorkspace "4")
  ,((fromList [WlcBitModAlt],keysym_5),return $ ViewWorkspace "5")
  ,((fromList [WlcBitModAlt],keysym_6),return $ ViewWorkspace "6")
  ,((fromList [WlcBitModAlt],keysym_7),return $ ViewWorkspace "7")
  ,((fromList [WlcBitModAlt],keysym_8),return $ ViewWorkspace "8")
  ,((fromList [WlcBitModAlt],keysym_9),return $ ViewWorkspace "9")
  ,((fromList [WlcBitModAlt],keysym_s),return Split)
  ,((fromList [WlcBitModAlt],keysym_d),return (Move Down))
  ,((fromList [WlcBitModAlt],keysym_u),return (Move Up))
  ,((fromList [WlcBitModAlt],keysym_space),return $ Cycle)
  ,((fromList [WlcBitModAlt],keysym_i),return $ MoveViewUp)
  ,((fromList [WlcBitModAlt,WlcBitModShift],keysym_X),return $ Close)]

-- | react to a new view
viewCreated :: (Reflex t,MonadHold t m,MonadFix m)
                     => Event t ViewCreated
                     -> m (Event t Actions)
viewCreated =
  return .
  fmap (\(ViewCreated view output) ->
          [InsertView view output,
          FocusView view])

-- | react to destroyed view
viewDestroyed :: (Reflex t,MonadHold t m,MonadFix m)
              => Event t ViewDestroyed
              -> m (Event t Actions)
viewDestroyed =
  return .
  fmap (\(ViewDestroyed view) ->
          return $ DestroyView view)

-- | react to new output
outputCreated :: (Reflex t,MonadHold t m,MonadFix m)
                       => Event t OutputCreated
                       -> m (Event t Actions)
outputCreated =
  return .
  fmap (\(OutputCreated output res) ->
          return $ CreateOutput output res)

-- | react to destroyed output
outputDestroyed :: (Reflex t,MonadHold t m,MonadFix m)
                => Event t OutputDestroyed -> m (Event t Actions)
outputDestroyed =
  return .
  fmap (\(OutputDestroyed output) -> return $ DestroyOutput output)

outputResolution :: (Reflex t,MonadHold t m,MonadFix m)
                 => Event t OutputResolution
                 -> m (Event t Actions)
outputResolution =
  return .
  fmap (\(OutputResolution output _ new) ->
          return $ ChangeResolution output new)
