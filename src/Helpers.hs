module Helpers
  ( getSym
  , getKeyState
  , getModifiers
  , emptyStackSet
  , singleton'
  , modToWLCMod
  ) where

import           Data.Bits
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Sum
import           Data.GADT.Compare
import           Data.Set hiding (filter)
import           EmacsKeys
import           Foreign.C.Types
import           Text.XkbCommon
import           WLC

import           LayoutType
import           StackSet
import           Tree

getSym :: CUInt -> Keysym
getSym sym = Keysym (fromIntegral sym)

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

-- | generate singleton map from dsum.
singleton' :: GCompare k => DSum k -> DMap.DMap k
singleton' = DMap.fromList . (:[])

modToWLCMod :: Modifier -> WLCModifier
modToWLCMod Shift = WlcBitModShift
modToWLCMod Meta = WlcBitModAlt
modToWLCMod Ctrl = WlcBitModCtrl
