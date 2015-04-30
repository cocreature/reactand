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
  outputCreatedW <- wrapCreated (oCreated messages action)
  _ <- wlcInit (def & WLC.keyboard . keyboardKey .~ keyboardKeyW
                    & WLC.view . viewCreated .~ viewCreatedW
                    & WLC.view . viewFocus .~ viewFocusW
                    & WLC.output . outputCreated .~ outputCreatedW) []
  wlcRun
  wlcTerminate
  freeHaskellFunPtr keyboardKeyW
  freeHaskellFunPtr viewCreatedW
  freeHaskellFunPtr viewFocusW

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
  do output <- wlcViewGetOutput view
     writeChan messages
               (TViewCreated :=>
                ViewCreated view output)
     act <- takeMVar action
     act
     return 1

vFocus :: WLCHandle -> CBool -> IO ()
vFocus view focus =
  do wlcViewSetState view
                     WlcBitActivated
                     (focus /= 0)

oCreated :: Chan (DSum Tag) -> MVar (IO ()) -> WLCHandle -> IO CBool
oCreated messages action output =
         do writeChan messages (TOutputCreated :=> OutputCreated output)
            act <- takeMVar action
            act
            return 1
