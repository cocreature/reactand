{-# LANGUAGE RankNTypes #-}

module Reactand.Host
  ( host
  ) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

import Reactand.Types

-- | reflex host for the window manager
host :: Chan (DSum Tag) -> MVar (IO ()) -> (forall t m. WindowManager t m) -> IO ()
host messages action wm =
  runSpiderHost $
  do (e,eTriggerRef) <- newEventWithTriggerRef
     b <- runHostFrame $ wm e
     handle <- subscribeEvent b -- bennofs said that I should do this
     forever $
       do message <- liftIO $ readChan messages
          mETrigger <- liftIO $ readIORef eTriggerRef
          case mETrigger of
            Nothing -> return () -- nobody cares about our input
            Just eTrigger ->
              (liftIO =<<) $
              fireEventsAndRead [eTrigger :=> message] $ -- pass input
              do r <- readEvent handle -- read the output
                 case r of
                   Nothing ->
                     -- fill tmvar for callbacks
                     return $ putMVar action (return ())
                   Just act ->
                     -- put the action inside a tmvar for execution by callback
                     do (putMVar action <$> act)
