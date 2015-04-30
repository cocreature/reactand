{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Types
  ( Tag(..)
  , Key(..)
  , ViewCreated(..)
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

data Tag a where
     TKey :: Tag Key
     TViewCreated :: Tag ViewCreated
     TOutputCreated :: Tag OutputCreated

instance GEq Tag where
  geq TKey TKey = Just Refl
  geq TViewCreated TViewCreated = Just Refl
  geq TOutputCreated TOutputCreated = Just Refl
  geq _ _ = Nothing

instance GCompare (Tag) where
  gcompare TKey TKey = GEQ
  gcompare TKey _ = GLT
  gcompare TViewCreated TKey = GGT
  gcompare TViewCreated TViewCreated = GEQ
  gcompare TViewCreated TOutputCreated = GLT
  gcompare TOutputCreated TKey = GGT
  gcompare TOutputCreated TViewCreated = GGT
  gcompare TOutputCreated TOutputCreated = GEQ

data Key =
  Key WLCKeyState
      Keysym
      (Set WLCModifier)
  deriving (Show,Eq)

data ViewCreated =
  ViewCreated WLCHandle WLCHandle
  deriving (Show,Eq,Ord)

data OutputCreated = OutputCreated WLCHandle deriving (Show,Eq,Ord)

type WindowManager t m = (Reflex t,MonadHold t m,MonadFix m) => Event t (DSum Tag) -> m (Event t (IO ()))
type StackSetChange i l a sid = StackSet i l a sid -> StackSet i l a sid
