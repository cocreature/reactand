module Helpers
  ( getSym
  , getKeyState
  , getModifiers
  , emptyStackSet
  , singleton'
  ) where

import           Data.Bits
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Sum
import           Data.Set hiding (filter)
import           Foreign.C.Types
import           Text.XkbCommon
import           WLC
import           Data.GADT.Compare

import           Layout
import           StackSet

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

emptyStackSet :: StackSet String (DefaultLayout w) a sid
emptyStackSet =
  StackSet Nothing
           []
           (fmap (\i ->
                    (Workspace (show i)
                               (DefaultLayout)
                               (2 ^ i)
                               Nothing))
                 [0 :: Int .. 9])

-- | generate singleton map from dsum.
singleton' :: GCompare k => DSum k -> DMap.DMap k
singleton' = DMap.fromList . (:[])
