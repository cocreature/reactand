{-# LANGUAGE RankNTypes #-}
module Reactand.LayoutType
       (Layout(..)
       ,tabbedLayout
       ,horizontalLayout
       ,verticalLayout
       ,tallLayout
       ,wideLayout)
       where

import Text.PrettyPrint.HughesPJClass
import Foreign.C.Types
import WLC

import Reactand.Tree

data Layout =
  Layout {getLayout :: forall a. WLCSize -> ListZipper a -> [(a,WLCGeometry)]
         ,name :: String}

instance Eq Layout where
  (Layout _ s) == (Layout _ s') = s == s'

instance Show Layout where
  show (Layout _ s) = "Layout " ++ s

instance Pretty Layout where
  pPrint (Layout _ s) = text "Layout " <+> text s

tabbedLayout :: Layout
tabbedLayout =
  Layout (\size' stack ->
            let stackList = integrate stack
            in zip stackList (repeat (WLCGeometry (WLCOrigin 0 0) size')))
         "Tabbed"

horizontalLayout :: Layout
horizontalLayout = simpleLayout horizontalSplit "Horizontal"

horizontalSplit :: CInt -> CInt -> WLCSize -> WLCGeometry
horizontalSplit total i (WLCSize w h)
  | total == 0 = error "No windows found"
  | otherwise = WLCGeometry (WLCOrigin (i * fromIntegral deltaX) 0) (WLCSize deltaX h)
  where deltaX = w `div` fromIntegral total

verticalLayout :: Layout
verticalLayout = simpleLayout verticalSplit "Vertical"

verticalSplit :: CInt -> CInt -> WLCSize -> WLCGeometry
verticalSplit total i (WLCSize w h)
  | total == 0 = error "No windows found"
  | otherwise = WLCGeometry (WLCOrigin 0 (i * fromIntegral deltaY)) (WLCSize w deltaY)
  where deltaY = h `div` fromIntegral total

tallLayout :: Layout
tallLayout = simpleLayout tallSplit "Tall"

tallSplit :: CInt -> CInt -> WLCSize -> WLCGeometry
tallSplit total i (WLCSize w h)
  | total == 0 = error "No windows found"
  | i == 0 =
    WLCGeometry (WLCOrigin 0 0)
                (WLCSize (w `div` (if total == 1 then 1 else 2)) h)
  | otherwise =
    WLCGeometry
      (WLCOrigin w'
                 ((i - 1) *
                  fromIntegral deltaY))
      (WLCSize (fromIntegral w') deltaY)
  where deltaY =
          h `div`
          (fromIntegral total - 1)
        w' = fromIntegral w `div` 2

wideLayout :: Layout
wideLayout = simpleLayout wideSplit "Wide"

wideSplit :: CInt -> CInt -> WLCSize -> WLCGeometry
wideSplit total i (WLCSize w h)
  | total == 0 = error "No windows found"
  | i == 0 = WLCGeometry (WLCOrigin 0 0) (WLCSize w (h `div` (if total == 1 then 1 else 2)))
  | otherwise = WLCGeometry (WLCOrigin ((i-1) * fromIntegral deltaX) h') (WLCSize deltaX (fromIntegral h'))
  where deltaX = w `div` (fromIntegral total -1)
        h' = fromIntegral h `div` 2

simpleLayout :: (CInt -> CInt -> WLCSize -> WLCGeometry) -> String -> Layout
simpleLayout f s = Layout (\size' stack ->
                                  let stackList = integrate stack
                                  in zip stackList
                                         (map (\i ->
                                                 f (fromIntegral $ length stackList) i size')
                                              [0 ..]))
                               s
