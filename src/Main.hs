{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import qualified Data.Set as Set
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Lens hiding (view)
import           Control.Monad.State.Strict
import           MVC hiding (spawn)
import qualified MVC.Prelude as MVC
import qualified System.Process as Process

import           Reactand.Helpers
import           Reactand.Keyhandler
import           Reactand.Layout
import           Reactand.LayoutType
import           Reactand.StackSet hiding (modify)
import           Reactand.Tree
import           Reactand.Types
import           Reactand.WLCHandlers
import qualified Text.PrettyPrint.HughesPJClass as PP
import           Text.XkbCommon hiding (model,layout)
import           WLC

main :: IO ()
main =
  do messages <- newChan
     action <- newEmptyMVar
     asyncWLC <- async (runWLC messages action)
     _ <- runMVC emptyStackSet model (external messages action)
     (_,_) <-
       waitAnyCatchCancel [asyncWLC]
     return ()

external :: Chan Event
         -> MVar (IO ())
         -> Managed (View (String,
                          StackSet String WLCViewPtr WLCOutputPtr,[Command]),
                          Controller Event)
external messages action =
  do c <-
       MVC.producer (bounded 1)
                    (chanProducer messages)
     renderView <- MVC.consumer (renderConsumer action)
     spawnView <- MVC.consumer spawnConsumer
     return (handles _1 debugView <> handles _2 renderView <> handles _3 spawnView,c)

spawnConsumer :: Consumer [Command] IO ()
spawnConsumer = do cmds <- await
                   liftIO $ mapM_ handleCommand cmds
                   spawnConsumer

handleCommand :: Command -> IO ()
handleCommand (Run x) = void $ Process.spawnCommand x

model :: Model (StackSet String WLCViewPtr WLCOutputPtr)
               Event
               (String,StackSet String WLCViewPtr WLCOutputPtr,[Command])
model = asPipe modelPipe

chanProducer :: Chan a -> Producer a IO ()
chanProducer chan =
  do a <- lift $ readChan chan
     yield a
     chanProducer chan

debugView :: View String
debugView = MVC.stdoutLines

renderConsumer :: PP.Pretty (StackSet i WLCViewPtr WLCOutputPtr)
               => MVar (IO ())
               -> Consumer (StackSet i WLCViewPtr WLCOutputPtr)
                           IO
                           ()
renderConsumer action = do
  s <- await
  liftIO $ putMVar action (return ())
  liftIO $ relayout s
  liftIO $ print (PP.pPrint s)
  renderConsumer action

modelPipe :: Pipe Event
                  (String,StackSet String WLCViewPtr WLCOutputPtr,[Command])
                  (State (StackSet String WLCViewPtr WLCOutputPtr)) ()
modelPipe = do
  e <- await
  s <- get
  let (s',cmds) = handleEvent e s
  put s'
  yield (show e,s',cmds)
  modelPipe

handleEvent :: Event
            -> StackSet String WLCViewPtr WLCOutputPtr
            -> (StackSet String WLCViewPtr WLCOutputPtr,[Command])
handleEvent (EvOutputCreated (OutputCreated out res)) s =
  (createOutput out res s,[])
handleEvent (EvOutputResolution (OutputResolution out _ newRes)) s =
  (changeResolution out newRes s,[])
handleEvent (EvOutputDestroyed (OutputDestroyed out)) s = (removeOutput out s,[])
handleEvent (EvViewCreated (ViewCreated view out)) s =
  (insertViewInOutput horizontalLayout view out s,[])
handleEvent (EvViewDestroyed (ViewDestroyed v)) s = (deleteFromStackSet v s,[])
handleEvent (EvKey k) s = handleKey k s
