module Reactand.Helpers
  ( getSym
  , getKeyState
  , getModifiers
  , emptyStackSet
  , modToWLCMod
  ) where

import           Data.Bits
import           Data.Set hiding (filter)
import           EmacsKeys
import           Foreign.C.Types
import           Text.XkbCommon
import           WLC

import           Reactand.LayoutType
import           Reactand.StackSet
import           Reactand.Tree

getSym :: CUInt -> WLCModifiersPtr -> IO Keysym
getSym = wlcKeyboardGetKeysymForKey

getKeyState :: WLCKeyStateBit -> WLCKeyState
getKeyState b = toEnum (fromIntegral b)

getModifiers :: WLCModifiers -> Set WLCModifier
getModifiers (WLCModifiers _ mods) =
  fromList (filter (\modifier ->
                      mods .&.
                      fromIntegral (fromEnum modifier) /=
                      0)
                   (enumFrom WlcBitModShift))

emptyStackSet :: StackSet String a sid
emptyStackSet =
  StackSet Nothing
           []
           (fmap (\i ->
                    (Workspace (show i)
                               (2 ^ i)
                               (TreeZipper (Tree horizontalLayout Nothing)
                                           [])))
                 [0 :: Int .. 1])

modToWLCMod :: Modifier -> WLCModifier
modToWLCMod Shift = WlcBitModShift
modToWLCMod Meta = WlcBitModAlt
modToWLCMod Ctrl = WlcBitModCtrl
