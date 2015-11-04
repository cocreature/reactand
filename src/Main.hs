{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad.State.Strict
import           MVC
import qualified MVC.Prelude as MVC
import qualified Pipes.Prelude as Pipes

import           Reactand.WLCHandlers
import           Reactand.Types

main :: IO ()
main =
  do messages <- newChan
     action <- newEmptyMVar
     asyncWLC <- async (runWLC messages action)
     runMVC () model (external messages action)
     (_,_) <-
       waitAnyCatchCancel [asyncWLC]
     return ()

external
  :: Chan Event -> MVar (IO ()) -> Managed (View String,Controller Event)
external messages action =
  do c <-
       MVC.producer (bounded 1)
                    (chanProducer messages)
     return (MVC.stdoutLines <> asSink (const (putMVar action (return ()))),c)

model :: Model () Event String
model = asPipe modelPipe

chanProducer :: Chan a -> Producer a IO ()
chanProducer chan =
  do a <- lift $ readChan chan
     yield a
     chanProducer chan


modelPipe :: Pipe Event String (State ()) ()
modelPipe = Pipes.map show
