module WLCHandlers
  ( runWLC
  ) where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Lens hiding (view)
import           Data.Default
import           Data.Dependent.Sum
import           Foreign
import           Foreign.C.Types
import           WLC
import qualified WLC.Lenses as WLC
import           WLC.Lenses hiding (view,output)
import           WLC.Wrapper

import           Helpers
import           Types

runWLC :: Chan (DSum Tag) -> MVar (IO ()) -> IO ()
runWLC messages action = do
  keyboardKeyW <- wrapKey (kKey messages action)
  viewCreatedW <- wrapCreated (vCreated messages action)
  viewFocusW <- wrapFocus vFocus
  viewDestroyedW <- wrapDestroyed (vDestroyed messages action)
  outputCreatedW <- wrapCreated (oCreated messages action)
  outputDestroyedW <- wrapDestroyed (oDestroyed messages action)
  outputResolutionW <- wrapResolution (oResolution messages action)
  _ <- wlcInit (def & WLC.keyboard . keyboardKey .~ keyboardKeyW
                    & WLC.view . viewCreated .~ viewCreatedW
                    & WLC.view . viewFocus .~ viewFocusW
                    & WLC.view . viewDestroyed .~ viewDestroyedW
                    & WLC.output . outputCreated .~ outputCreatedW
                    & WLC.output . outputDestroyed .~ outputDestroyedW
                    & WLC.output . outputResolution .~ outputResolutionW) []
  wlcRun
  wlcTerminate
  freeHaskellFunPtr keyboardKeyW
  freeHaskellFunPtr viewCreatedW
  freeHaskellFunPtr viewFocusW
  freeHaskellFunPtr viewDestroyedW
  freeHaskellFunPtr outputCreatedW
  freeHaskellFunPtr outputDestroyedW

kKey :: Chan (DSum Tag)
     -> MVar (IO ())
     -> WLCHandle
     -> CUInt
     -> WLCModifiersPtr
     -> CUInt
     -> CUInt
     -> WLCKeyStateBit
     -> IO CBool
kKey messages action _view _ modifiersPtr _ symBit keyStateBit =
  do mods <- getModifiers <$> modifiers
     writeChan messages
               (TKey :=>
                Key (getKeyState keyStateBit) sym mods)
     act <- takeMVar action
     act
     return 1
  where sym = getSym symBit
        modifiers = peek modifiersPtr

vCreated :: Chan (DSum Tag) -> MVar (IO ()) -> WLCHandle -> IO CBool
vCreated messages action view =
  do output <- wlcViewGetOutput (WLCViewPtr view)
     writeChan messages
               (TViewCreated :=>
                ViewCreated (WLCViewPtr view) (WLCOutputPtr output))
     act <- takeMVar action
     act
     return 1

vFocus :: WLCHandle -> CBool -> IO ()
vFocus view focus =
  do wlcViewSetState (WLCViewPtr view)
                     WlcBitActivated
                     (focus /= 0)

vDestroyed :: Chan (DSum Tag) -> MVar (IO ()) -> WLCHandle -> IO ()
vDestroyed messages action view =
  do writeChan messages (TViewDestroyed :=> ViewDestroyed (WLCViewPtr view))
     act <- takeMVar action
     act

oCreated :: Chan (DSum Tag) -> MVar (IO ()) -> WLCHandle -> IO CBool
oCreated messages action output =
  do writeChan messages
               (TOutputCreated :=>
                OutputCreated (WLCOutputPtr output))
     act <- takeMVar action
     act
     return 1

oDestroyed :: Chan (DSum Tag) -> MVar (IO ()) -> WLCHandle -> IO ()
oDestroyed messages action output =
  do writeChan messages
               (TOutputDestroyed :=>
                OutputDestroyed (WLCOutputPtr output))
     act <- takeMVar action
     act

oResolution :: Chan (DSum Tag) -> MVar (IO ()) -> WLCHandle -> WLCSizePtr -> WLCSizePtr -> IO ()
oResolution messages action output old new =
  do old' <- peek old
     new' <- peek new
     writeChan messages (TOutputResolution :=> OutputResolution (WLCOutputPtr output) old' new')
     act <- takeMVar action
     act
