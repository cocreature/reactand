{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Types
  ( Tag(..)
  , Key(..)
  , ViewCreated(..)
  , ViewDestroyed(..)
  , OutputCreated(..)
  , OutputDestroyed(..)
  , OutputResolution(..)
  , WindowManager
  , Actions
  , Dir(..)
  , Action(..)
  ) where

import Control.Monad.Fix
import Data.Dependent.Map hiding (Key,split)
import Data.GADT.Compare.TH
import Data.Set hiding (split)
import Reflex
import Text.XkbCommon
import WLC

data Tag a where
     TKey :: Tag Key
     TViewCreated :: Tag ViewCreated
     TViewDestroyed :: Tag ViewDestroyed
     TOutputCreated :: Tag OutputCreated
     TOutputDestroyed :: Tag OutputDestroyed
     TOutputResolution :: Tag OutputResolution

data Key =
  Key WLCKeyState
      Keysym
      (Set WLCModifier)
  deriving (Show,Eq)

data ViewCreated =
  ViewCreated WLCViewPtr WLCOutputPtr
  deriving (Show,Eq,Ord)

data ViewDestroyed = ViewDestroyed WLCViewPtr deriving (Show,Eq,Ord)

data OutputCreated = OutputCreated WLCOutputPtr WLCSize deriving (Show,Eq,Ord)

data OutputDestroyed = OutputDestroyed WLCOutputPtr deriving (Show,Eq,Ord)

data OutputResolution = OutputResolution WLCOutputPtr WLCSize WLCSize deriving (Show,Eq,Ord)

type WindowManager t m = (Reflex t,MonadHold t m,MonadFix m) => Event t (DSum Tag) -> m (Event t (IO ()))

deriveGEq ''Tag
deriveGCompare ''Tag

data Action
  = InsertView WLCViewPtr
               WLCOutputPtr
  | FocusView WLCViewPtr
  | DestroyView WLCViewPtr
  | CreateOutput WLCOutputPtr WLCSize
  | DestroyOutput WLCOutputPtr
  | SpawnCommand String
  | Focus Dir
  | Swap Dir
  | Output Dir
  | Move Dir
  | Split
  | ViewWorkspace String
  | ChangeResolution WLCOutputPtr WLCSize
  | Cycle
  | MoveViewUp
  | Close

type Actions = [Action]

data Dir = Up | Down
