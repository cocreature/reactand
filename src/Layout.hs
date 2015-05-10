{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Layout 
  (DefaultLayout(..)
  ,LayoutClass
  ,relayout
  ,insertViewInOutput
  ) where

import Control.Lens
import Foreign.C.Types
import StackSet
import Data.List
import WLC hiding (size)
import Tree

class (Show layout) => LayoutClass layout where
  pureLayout :: layout -> WLCSize -> ListZipper a -> [(a,WLCGeometry)]

data DefaultLayout = DefaultLayout deriving (Show,Eq,Ord)

instance LayoutClass DefaultLayout where
  pureLayout _ size stack =
    zip stackList
        (map (\(_,i) ->
           calculateGeometry (genericLength stackList)
                             i
                             size)
           (zip stackList [0 ..]))
    where stackList = integrate stack

calculateGeometry :: CInt -> CInt -> WLCSize -> WLCGeometry
calculateGeometry total i (WLCSize resW resH)
  | total == 0 = error "No windows found"
  | i == total - 1 && i `mod` 2 == 0 = WLCGeometry (WLCOrigin x y) (WLCSize resW h)
  | otherwise = WLCGeometry (WLCOrigin x y) (WLCSize w h)
  where x = (i `mod` 2) * (fromIntegral resW `div` 2)
        y = (i `div` 2) * (fromIntegral resH `div` ((total + 1) `div` 2))
        w = resW `div` 2
        h = resH `div` ((fromIntegral total + 1) `div` 2)

relayout :: LayoutClass l
         => StackSet i l WLCViewPtr WLCOutputPtr -> IO ()
relayout s = do
  forOf_ (current . _Just) s layoutScreen
  forOf_ (current . _Just . workspace . tree . focusT . treeElements . _Just . focusL . _Left) s wlcViewFocus
  forOf_ (visible . each) s layoutScreen

layoutScreen :: LayoutClass l
             => Screen i l WLCViewPtr WLCOutputPtr -> IO ()
layoutScreen (Screen w sid) = do
  res <- wlcOutputGetResolution sid
  wlcOutputSetMask sid (w ^. mask)
  layoutWorkspace res w

-- | Resize all views on the workspace according to the current layout
layoutWorkspace :: LayoutClass l
                => WLCSize -> Workspace i l WLCViewPtr -> IO ()
layoutWorkspace size ws =
  layoutTree size
             (integrateTree (ws ^. tree))

layoutTree :: LayoutClass l => WLCSize -> Tree l WLCViewPtr -> IO ()
layoutTree screenSize mainTree =
  go (WLCGeometry (WLCOrigin 0 0)
                  screenSize)
     mainTree
  where go _ (Tree _ Nothing) = return ()
        go (WLCGeometry (WLCOrigin x y) size) (Tree l (Just z)) =
          let arrangement =
                map (over _2
                          (\(WLCGeometry (WLCOrigin x' y') size') ->
                             WLCGeometry
                               (WLCOrigin (x + x')
                                          (y + y'))
                               size')) $
                pureLayout l size z
          in mapM_ recurse arrangement
        recurse ((Left v),geometry) =
          wlcViewSetGeometry v geometry >>
          wlcViewSetState v WlcBitMaximized True
        recurse ((Right t),geometry) =
          go geometry t


-- | insert the view into workspace that is focused on the output
insertViewInOutput :: l
                   -> StackSet i l WLCViewPtr WLCOutputPtr
                   -> WLCViewPtr
                   -> WLCOutputPtr
                   -> StackSet i l WLCViewPtr WLCOutputPtr
insertViewInOutput l s v output =
  modifyWithOutput (insertUp l v)
                   output
                   s
