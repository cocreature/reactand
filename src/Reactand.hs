{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Reactand where

import           Control.Lens hiding (view)
import           Control.Monad
import           Control.Monad.Fix
import           Data.Bifunctor
import           Data.Set hiding (map,filter,foldr,split)
import           EmacsKeys
import           Reflex
import qualified System.Process as P
import           Text.PrettyPrint.HughesPJClass hiding (first)
import           Text.XkbCommon
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
keyHandlers = fmap (first (bimap (fromList . fmap modToWLCMod) head)) $
  [($(mkEmacsKeys "M-Return"),return $ SpawnCommand "weston-terminal")
  ,($(mkEmacsKeys "M-n"),return (Focus Down))
  ,($(mkEmacsKeys "M-r"),return (Focus Up))
  ,($(mkEmacsKeys "M-S-n"),return (Swap Down))
  ,($(mkEmacsKeys "M-S-r"),return (Swap Up))
  ,($(mkEmacsKeys "M-e"),return $ (Output Up))
  ,($(mkEmacsKeys "M-a"),return $ (Output Down))
  ,($(mkEmacsKeys "M-0"),return $ ViewWorkspace "0")
  ,($(mkEmacsKeys "M-1"),return $ ViewWorkspace "1")
  ,($(mkEmacsKeys "M-2"),return $ ViewWorkspace "2")
  ,($(mkEmacsKeys "M-3"),return $ ViewWorkspace "3")
  ,($(mkEmacsKeys "M-4"),return $ ViewWorkspace "4")
  ,($(mkEmacsKeys "M-5"),return $ ViewWorkspace "5")
  ,($(mkEmacsKeys "M-6"),return $ ViewWorkspace "6")
  ,($(mkEmacsKeys "M-7"),return $ ViewWorkspace "7")
  ,($(mkEmacsKeys "M-8"),return $ ViewWorkspace "8")
  ,($(mkEmacsKeys "M-9"),return $ ViewWorkspace "9")
  ,($(mkEmacsKeys "M-s"),return Split)
  ,($(mkEmacsKeys "M-d"),return (Move Down))
  ,($(mkEmacsKeys "M-u"),return (Move Up))
  ,($(mkEmacsKeys "M-space"),return $ Cycle)
  ,($(mkEmacsKeys "M-i"),return $ MoveViewUp)
  ,($(mkEmacsKeys "M-S-x"),return $ Close)]

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
