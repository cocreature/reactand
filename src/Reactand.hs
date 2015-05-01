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
     keyEv <- reactiveKey (select selector TKey)
     viewCreatedEv <-
       reactiveViewCreated (select selector TViewCreated)
     outputCreatedEv <-
       reactiveOutputCreated (select selector TOutputCreated)
     viewDestroyedEv <- viewDestroyed (select selector TViewDestroyed)
     let (stacksetChanges,actions) =
           unzip . fmap splitE $
           [keyEv,viewCreatedEv,outputCreatedEv,viewDestroyedEv]
     stacksetChanges' <-
       -- apply accumulated changes to stackset
       mapDyn (\stackSet -> print stackSet >> relayout stackSet) =<<
       (nubDyn <$> foldDyn ($) emptyStackSet (mergeWith (.) stacksetChanges) :: m (Dynamic t (StackSet String (DefaultLayout WLCHandle) WLCHandle WLCHandle)))
     return $
       mergeWith (>>) (updated stacksetChanges' : actions)

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
                    -> m (Event t (StackSetChange String
                                                  (DefaultLayout WLCHandle)
                                                  WLCHandle CULong,
                                   IO ()))
reactiveViewCreated =
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

reactiveOutputCreated :: (Reflex t,MonadHold t m,MonadFix m)
                      => Event t OutputCreated
                      -> m (Event t (StackSetChange i l a WLCHandle,IO ()))
reactiveOutputCreated =
  return .
  fmap (\(OutputCreated output) ->
          (createOutput output,return ()))
