{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Types
  ( Tag(..)
  , Key(..)
  , ViewCreated(..)
  , ViewDestroyed(..)
  , OutputCreated(..)
  , WindowManager
  , StackSetChange
  ) where

import Control.Monad.Fix
import Data.Dependent.Map hiding (Key)
import Data.GADT.Compare
import Data.Set
import Reflex
import Text.XkbCommon
import WLC

import StackSet
import TH

data Tag a where
     TKey :: Tag Key
     TViewCreated :: Tag ViewCreated
     TViewDestroyed :: Tag ViewDestroyed
     TOutputCreated :: Tag OutputCreated

data Key =
  Key WLCKeyState
      Keysym
      (Set WLCModifier)
  deriving (Show,Eq)

data ViewCreated =
  ViewCreated WLCHandle WLCHandle
  deriving (Show,Eq,Ord)

data ViewDestroyed = ViewDestroyed WLCHandle

data OutputCreated = OutputCreated WLCHandle deriving (Show,Eq,Ord)

type WindowManager t m = (Reflex t,MonadHold t m,MonadFix m) => Event t (DSum Tag) -> m (Event t (IO ()))
type StackSetChange i l a sid = StackSet i l a sid -> StackSet i l a sid

makeGEqInstance ''Tag
makeGCompareInstance ''Tag
