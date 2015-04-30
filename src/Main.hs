{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Host
import Reactand
import WLCHandlers

main :: IO ()
main =
  do messages <- newChan
     action <- newEmptyMVar
     aWLC <- async (runWLC messages action)
     aReflex <- async (host messages action reactand)
     (_,_) <-
       waitAnyCatchCancel [aWLC,aReflex]
     return ()
