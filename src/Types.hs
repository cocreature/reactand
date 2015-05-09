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
  , WindowManager
  , StackSetChange
  ) where

import Control.Monad.Fix
import Data.Dependent.Map hiding (Key)
import Data.GADT.Compare.TH
import Data.Set
import Reflex
import Text.XkbCommon
import WLC

import StackSet

data Tag a where
     TKey :: Tag Key
     TViewCreated :: Tag ViewCreated
     TViewDestroyed :: Tag ViewDestroyed
     TOutputCreated :: Tag OutputCreated
     TOutputDestroyed :: Tag OutputDestroyed

data Key =
  Key WLCKeyState
      Keysym
      (Set WLCModifier)
  deriving (Show,Eq)

data ViewCreated =
  ViewCreated WLCViewPtr WLCOutputPtr
  deriving (Show,Eq,Ord)

data ViewDestroyed = ViewDestroyed WLCViewPtr deriving (Show,Eq,Ord)

data OutputCreated = OutputCreated WLCOutputPtr deriving (Show,Eq,Ord)

data OutputDestroyed = OutputDestroyed WLCOutputPtr deriving (Show,Eq,Ord)

type WindowManager t m = (Reflex t,MonadHold t m,MonadFix m) => Event t (DSum Tag) -> m (Event t (IO ()))
type StackSetChange i l a sid = StackSet i l a sid -> StackSet i l a sid

deriveGEq ''Tag
deriveGCompare ''Tag
