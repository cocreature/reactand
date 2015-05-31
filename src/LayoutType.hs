{-# LANGUAGE RankNTypes #-}
module LayoutType (Layout(..), defaultLayout, calculateGeometry,tabbedLayout)
       where

import Text.PrettyPrint.HughesPJClass
import Foreign.C.Types
import Tree
import WLC

data Layout =
  Layout {getLayout :: forall a. WLCSize -> ListZipper a -> [(a,WLCGeometry)]
         ,name :: String}

instance Eq Layout where
  (Layout _ s) == (Layout _ s') = s == s'

instance Show Layout where
  show (Layout _ s) = "Layout " ++ s

instance Pretty Layout where
  pPrint (Layout _ s) = text "Layout " <+> text s

defaultLayout :: Layout
defaultLayout =
  Layout (\size' stack ->
            let stackList = integrate stack
            in zip stackList
                   (map (\(_,i) ->
                           calculateGeometry (fromIntegral $ length stackList)
                                             i
                                             size')
                        (zip stackList [0 ..])))
         "Default"

calculateGeometry :: CInt -> CInt -> WLCSize -> WLCGeometry
calculateGeometry total i (WLCSize resW resH)
  | total == 0 = error "No windows found"
  | i == total - 1 && i `mod` 2 == 0 = WLCGeometry (WLCOrigin x y) (WLCSize resW h)
  | otherwise = WLCGeometry (WLCOrigin x y) (WLCSize w h)
  where x = (i `mod` 2) * (fromIntegral resW `div` 2)
        y = (i `div` 2) * (fromIntegral resH `div` ((total + 1) `div` 2))
        w = resW `div` 2
        h = resH `div` ((fromIntegral total + 1) `div` 2)

tabbedLayout :: Layout
tabbedLayout =
  Layout (\size' stack ->
            let stackList = integrate stack
            in zip stackList (repeat (WLCGeometry (WLCOrigin 0 0) size')))
         "Tabbed"
