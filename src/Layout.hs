{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Layout 
  (DefaultLayout(..)
  ,LayoutClass
  ,relayout
  ,insertViewInOutput
  ) where

import Foreign.C.Types
import StackSet
import Data.List
import WLC hiding (size)

class (Show (layout a)) => LayoutClass layout a where
  pureLayout :: layout a -> WLCSize -> Stack a -> [(a,WLCGeometry)]

data DefaultLayout a = DefaultLayout deriving (Show,Eq,Ord)

instance (Show a) => LayoutClass DefaultLayout a where
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

relayout :: LayoutClass l WLCViewPtr
         => StackSet i (l WLCViewPtr) WLCViewPtr WLCOutputPtr -> IO ()
relayout (StackSet current visible _) = do
  mapM_ layoutScreen current
  mapM_ (wlcViewFocus . focus) (stack . workspace =<< current)
  mapM_ layoutScreen visible

layoutScreen :: LayoutClass l WLCViewPtr
             => Screen i (l WLCViewPtr) WLCViewPtr WLCOutputPtr -> IO ()
layoutScreen (Screen w sid) = do
  res <- wlcOutputGetResolution sid
  wlcOutputSetMask sid (mask w)
  layoutWorkspace res w

-- | Resize all views on the workspace according to the current layout
layoutWorkspace :: LayoutClass l WLCViewPtr
                => WLCSize -> Workspace i (l WLCViewPtr) WLCViewPtr -> IO ()
layoutWorkspace _ (Workspace _ _ _ Nothing) = return ()
layoutWorkspace size' (Workspace _ layout mask (Just a)) = do
  mapM_ (\(view,geometry) ->
           do wlcViewSetState view WlcBitMaximized True
              wlcViewSetGeometry view geometry
              wlcViewSetMask view mask)
        (pureLayout layout size' a)

-- | insert the view into workspace that is focused on the output
insertViewInOutput :: StackSet i l WLCViewPtr WLCOutputPtr
                   -> WLCViewPtr
                   -> WLCOutputPtr
                   -> StackSet i l WLCViewPtr WLCOutputPtr
insertViewInOutput s view output =
  modifyWithOutput (Just (Stack view [] []))
                   (return . insertUp view)
                   output
                   s
