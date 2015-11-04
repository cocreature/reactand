{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Reactand.WLCHandlers
  ( runWLC
  ) where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Lens hiding (view)
import           Data.Default
import           Foreign hiding (new)
import           Foreign.C.Types
import           WLC
import qualified WLC.Lenses as WLC
import           WLC.Lenses hiding (view,output)
import           WLC.Wrapper

import           Reactand.Helpers
import           Reactand.Types

runWLC :: Chan Event -> MVar (IO ()) -> IO ()
runWLC messages action = do
  keyboardKeyW <- wrapKey (kKey messages action)
  viewCreatedW <- wrapCreated (vCreated messages action)
  viewFocusW <- wrapFocus vFocus
  viewDestroyedW <- wrapDestroyed (vDestroyed messages action)
  outputCreatedW <- wrapCreated (oCreated messages action)
  outputDestroyedW <- wrapDestroyed (oDestroyed messages action)
  outputResolutionW <- wrapResolution (oResolution messages action)
  pointerMotionW <- wrapMotion pMotion
  _ <- wlcInit (def & WLC.keyboard . keyboardKey .~ keyboardKeyW
                    & WLC.view . viewCreated .~ viewCreatedW
                    & WLC.view . viewFocus .~ viewFocusW
                    & WLC.view . viewDestroyed .~ viewDestroyedW
                    & WLC.output . outputCreated .~ outputCreatedW
                    & WLC.output . outputDestroyed .~ outputDestroyedW
                    & WLC.output . outputResolution .~ outputResolutionW
                    & WLC.pointer . pointerMotion .~ pointerMotionW) []
  wlcRun
  wlcTerminate
  freeHaskellFunPtr keyboardKeyW
  freeHaskellFunPtr viewCreatedW
  freeHaskellFunPtr viewFocusW
  freeHaskellFunPtr viewDestroyedW
  freeHaskellFunPtr outputCreatedW
  freeHaskellFunPtr outputDestroyedW

pMotion :: WLCHandle -> CUInt -> WLCOriginPtr -> IO CBool
pMotion view time origin = wlcPointerSetOrigin origin >> return 0

-- pattern CTrue <- ((0/=) -> True)
--   CTrue = 0

kKey :: Chan Event
     -> MVar (IO ())
     -> WLCHandle
     -> CUInt
     -> WLCModifiersPtr
     -> CUInt
     -> WLCKeyStateBit
     -> IO CBool
kKey messages action _view _ modifiersPtr symBit keyStateBit =
  do mods <- getModifiers <$> modifiers
     sym <- symIO
     writeChan messages
               (EvKey $
                Key (getKeyState keyStateBit) sym mods)
     act <- takeMVar action
     act
     return 1
  where symIO = getSym symBit modifiersPtr
        modifiers = peek modifiersPtr

vCreated :: Chan Event -> MVar (IO ()) -> WLCHandle -> IO CBool
vCreated messages action view =
  do output <- wlcViewGetOutput (WLCViewPtr view)
     writeChan messages
               (EvViewCreated $
                ViewCreated (WLCViewPtr view)
                            (WLCOutputPtr output))
     act <- takeMVar action
     act
     return 1

vFocus :: WLCHandle -> CBool -> IO ()
vFocus view focus =
  do wlcViewSetState (WLCViewPtr view)
                     WlcBitActivated
                     (focus /= 0)

vDestroyed :: Chan Event -> MVar (IO ()) -> WLCHandle -> IO ()
vDestroyed messages action view =
  do writeChan messages (EvViewDestroyed $ ViewDestroyed (WLCViewPtr view))
     act <- takeMVar action
     act

oCreated :: Chan Event -> MVar (IO ()) -> WLCHandle -> IO CBool
oCreated messages action output =
  do res <- wlcOutputGetResolution (WLCOutputPtr output)
     writeChan messages
               (EvOutputCreated $
                OutputCreated (WLCOutputPtr output)
                              res)
     act <- takeMVar action
     act
     return 1

oDestroyed :: Chan Event -> MVar (IO ()) -> WLCHandle -> IO ()
oDestroyed messages action output =
  do writeChan messages
               (EvOutputDestroyed $ OutputDestroyed (WLCOutputPtr output))
     act <- takeMVar action
     act

oResolution :: Chan Event -> MVar (IO ()) -> WLCHandle -> WLCSizePtr -> WLCSizePtr -> IO ()
oResolution messages action output old new =
  do old' <- peek old
     new' <- peek new
     writeChan messages
               (EvOutputResolution $
                OutputResolution (WLCOutputPtr output)
                                 old'
                                 new')
     act <- takeMVar action
     act
