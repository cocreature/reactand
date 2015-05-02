{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactand where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Set
import           Foreign.C.Types
import           Reflex
import           System.Process
import           Text.XkbCommon.KeysymList
import           WLC

import           Helpers
import           Layout
import           StackSet
import           Types

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

outputCreated :: (Reflex t,MonadHold t m,MonadFix m)
                       => Event t OutputCreated
                       -> m (Event t (StackSetChange i l a WLCHandle,IO ()))
outputCreated =
  return .
  fmap (\(OutputCreated output) ->
          (createOutput output,return ()))

outputDestroyed :: (Reflex t,MonadHold t m,MonadFix m) => Event t OutputDestroyed -> m (Event t (StackSetChange i l a WLCHandle, IO ()))
outputDestroyed =
  return .
  fmap (\(OutputDestroyed output) ->
          (removeOutput output,return ()))
