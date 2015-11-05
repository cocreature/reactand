{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Reactand.Types
  ( Key(..)
  , ViewCreated(..)
  , ViewDestroyed(..)
  , OutputCreated(..)
  , OutputDestroyed(..)
  , OutputResolution(..)
  , Actions
  , Dir(..)
  , Action(..)
  , Event(..)
  , Command(..)
  ) where

import Data.Set hiding (split)
import Text.XkbCommon
import WLC

data Event
  = EvKey Key
  | EvViewCreated ViewCreated
  | EvViewDestroyed ViewDestroyed
  | EvOutputCreated OutputCreated
  | EvOutputDestroyed OutputDestroyed
  | EvOutputResolution OutputResolution
  deriving (Show,Eq)

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

data Action
  = InsertView WLCViewPtr
               WLCOutputPtr
  | FocusView WLCViewPtr
  | DestroyView WLCViewPtr
  | CreateOutput WLCOutputPtr
                 WLCSize
  | DestroyOutput WLCOutputPtr
  | SpawnCommand String
  | Focus Dir
  | Swap Dir
  | Output Dir
  | Move Dir
  | Split
  | ViewWorkspace String
  | ChangeResolution WLCOutputPtr
                     WLCSize
  | Cycle
  | MoveViewUp
  | Close

type Actions = [Action]

data Dir = Up | Down

data Command = Run String
