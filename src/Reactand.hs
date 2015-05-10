{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactand where

import Control.Monad
import Control.Monad.Fix
import Data.Set hiding (map,filter,foldr)
import Reflex
import System.Process
import Text.XkbCommon.KeysymList
import Text.XkbCommon
import WLC

import Helpers
import Layout
import StackSet
import Types

-- | reactive window manager
reactand :: forall t m. WindowManager t m
reactand e =
  do let selector = fan (singleton' <$> e)
     keyEv <- key keyHandlers (select selector TKey)
     viewCreatedEv <-
       viewCreated (select selector TViewCreated)
     viewDestroyedEv <-
       viewDestroyed (select selector TViewDestroyed)
     outputCreatedEv <-
       outputCreated (select selector TOutputCreated)
     outputDestroyedEv <-
       outputDestroyed (select selector TOutputDestroyed)
     let (stacksetChanges,actions) =
           unzip . fmap splitE $
           [keyEv
           ,viewCreatedEv
           ,viewDestroyedEv
           ,outputCreatedEv
           ,outputDestroyedEv]
     stacksetChanges' <-
       -- apply accumulated changes to stackset
       mapDyn (\stackSet -> print stackSet >> relayout stackSet) =<<
       (nubDyn <$>
        foldDyn ($) emptyStackSet (mergeWith (.) stacksetChanges))
     return $
       mergeWith (>>) (updated stacksetChanges' : actions)

-- | react to key events
key :: (Reflex t,MonadHold t m,MonadFix m)
    => [((Set WLCModifier,Keysym),(StackSetChange i l a sid,IO ()))]
    -> Event t Key
    -> m (Event t (StackSetChange i l a sid,IO ()))
key handlers =
  return .
  fmapMaybe (\case
               Key WlcKeyStatePressed sym mods ->
                 foldr f Nothing $
                 map snd $
                 filter (\((mods',sym'),_) -> mods == mods' && sym == sym') handlers
               Key _ _ _ -> Nothing)
  where f (change,act) Nothing =
          Just (change,act)
        f (change,act) (Just (change',act')) =
          Just (change . change',act >> act')

keyHandlers :: Eq sid => [((Set WLCModifier,Keysym),(StackSetChange String l a sid,IO ()))]
keyHandlers =
  [((fromList [WlcBitModAlt],keysym_Return)
   ,(id
    ,void $
     spawnCommand "weston-terminal"))
  -- ,((fromList [WlcBitModAlt],keysym_n),(focusDown,return ()))
  -- ,((fromList [WlcBitModAlt],keysym_m),(focusUp,return ()))
  -- ,((fromList [WlcBitModAlt,WlcBitModShift],keysym_N),(swapDown,return ()))
  -- ,((fromList [WlcBitModAlt,WlcBitModShift],keysym_M),(swapUp,return ()))
  -- ,((fromList [WlcBitModAlt],keysym_0),(viewWorkspace "0",return ()))
  -- ,((fromList [WlcBitModAlt],keysym_1),(viewWorkspace "1",return ()))
  -- ,((fromList [WlcBitModAlt],keysym_2),(viewWorkspace "2",return ()))
  -- ,((fromList [WlcBitModAlt],keysym_3),(viewWorkspace "3",return ()))
  -- ,((fromList [WlcBitModAlt],keysym_4),(viewWorkspace "4",return ()))
  -- ,((fromList [WlcBitModAlt],keysym_5),(viewWorkspace "5",return ()))
  -- ,((fromList [WlcBitModAlt],keysym_6),(viewWorkspace "6",return ()))
  -- ,((fromList [WlcBitModAlt],keysym_7),(viewWorkspace "7",return ()))
  -- ,((fromList [WlcBitModAlt],keysym_8),(viewWorkspace "8",return ()))
  -- ,((fromList [WlcBitModAlt],keysym_9),(viewWorkspace "9",return ()))
  ]



-- | react to a new view
viewCreated :: (Reflex t,MonadHold t m,MonadFix m)
                     => Event t ViewCreated
                     -> m (Event t (StackSetChange String
                                                   DefaultLayout
                                                   WLCViewPtr WLCOutputPtr,
                                    IO ()))
viewCreated =
  return .
  fmap (\(ViewCreated view output) ->
               (\stackset ->
                       insertViewInOutput DefaultLayout stackset view output
                    ,wlcViewFocus view))

-- | react to destroyed view
viewDestroyed :: (Reflex t,MonadHold t m,MonadFix m)
              => Event t ViewDestroyed
              -> m (Event t (StackSetChange String
                                            DefaultLayout
                                            WLCViewPtr WLCOutputPtr,
                             IO ()))
viewDestroyed =
  return .
  fmap (\(ViewDestroyed view) ->
          (deleteFromStackSet view,return ()))

-- | react to new output
outputCreated :: (Reflex t,MonadHold t m,MonadFix m)
                       => Event t OutputCreated
                       -> m (Event t (StackSetChange i l a WLCOutputPtr,IO ()))
outputCreated =
  return .
  fmap (\(OutputCreated output) ->
          (createOutput output,return ()))

-- | react to destroyed output
outputDestroyed :: (Reflex t,MonadHold t m,MonadFix m) => Event t OutputDestroyed -> m (Event t (StackSetChange i l a WLCOutputPtr, IO ()))
outputDestroyed =
  return .
  fmap (\(OutputDestroyed output) ->
          (removeOutput output,return ()))
