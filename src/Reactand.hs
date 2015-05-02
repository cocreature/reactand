{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactand where

import Control.Monad
import Control.Monad.Fix
import Data.Set
import Debug.Trace
import Foreign.C.Types
import Reflex
import System.Process
import Text.XkbCommon.KeysymList
import WLC

import Helpers
import Layout
import StackSet
import Types

-- | reactive window manager
reactand :: forall t m. WindowManager t m
reactand e =
  do let selector = fan (singleton' <$> e)
     keyEv <- key (select selector TKey)
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
        foldDyn ($) emptyStackSet (mergeWith (.) stacksetChanges) :: m (Dynamic t (StackSet String (DefaultLayout WLCHandle) WLCHandle WLCHandle)))
     return $
       mergeWith (>>) (updated stacksetChanges' : actions)

-- | react to key events
key :: (Reflex t,MonadHold t m,MonadFix m)
             => Event t Key -> m (Event t (StackSetChange i l a sid,IO ()))
key =
  return .
  fmapMaybe (\case
               Key WlcKeyStatePressed sym mods
                 | mods ==
                     fromList [WlcBitModAlt] &&
                     sym == keysym_Return ->
                   Just (id
                        ,void $
                         spawnCommand "weston-terminal")
                 | mods ==
                     fromList [WlcBitModAlt] &&
                     sym == keysym_n ->
                   Just (focusDown,return ())
                 | mods ==
                     fromList [WlcBitModAlt] &&
                     sym == keysym_m ->
                   Just (focusUp,return ())
                 | mods ==
                     fromList [WlcBitModShift,WlcBitModAlt] &&
                     sym == keysym_N ->
                   Just (swapDown,return ())
                 | mods ==
                     fromList [WlcBitModShift,WlcBitModAlt] &&
                     sym == keysym_M ->
                   Just (swapUp,return ())
               Key _ sym mods -> trace (show mods ++ " " ++ show sym) $ Nothing)

-- | react to a new view
viewCreated :: (Reflex t,MonadHold t m,MonadFix m)
                     => Event t ViewCreated
                     -> m (Event t (StackSetChange String
                                                   (DefaultLayout WLCHandle)
                                                   WLCHandle CULong,
                                    IO ()))
viewCreated =
  return .
  fmap (\(ViewCreated view output) ->
               (\stackset ->
                       insertViewInOutput stackset view output
                    ,wlcViewFocus view))

-- | react to destroyed view
viewDestroyed :: (Reflex t,MonadHold t m,MonadFix m)
              => Event t ViewDestroyed
              -> m (Event t (StackSetChange String
                                            (DefaultLayout WLCHandle)
                                            WLCHandle CULong,
                             IO ()))
viewDestroyed =
  return .
  fmap (\(ViewDestroyed view) ->
          (deleteFromStackSet view,return ()))

-- | react to new output
outputCreated :: (Reflex t,MonadHold t m,MonadFix m)
                       => Event t OutputCreated
                       -> m (Event t (StackSetChange i l a WLCHandle,IO ()))
outputCreated =
  return .
  fmap (\(OutputCreated output) ->
          (createOutput output,return ()))

-- | react to destroyed output
outputDestroyed :: (Reflex t,MonadHold t m,MonadFix m) => Event t OutputDestroyed -> m (Event t (StackSetChange i l a WLCHandle, IO ()))
outputDestroyed =
  return .
  fmap (\(OutputDestroyed output) ->
          (removeOutput output,return ()))
