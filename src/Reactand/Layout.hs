{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Reactand.Layout (relayout, cycleLayout, insertViewInOutput) where

import Control.Lens
import WLC hiding (size)

import Reactand.StackSet
import Reactand.Tree
import Reactand.LayoutType

relayout :: StackSet i WLCViewPtr WLCOutputPtr -> IO ()
relayout s =
  do forOf_ (current . _Just) s layoutScreen
     forOf_ (visible . each) s layoutScreen
     forOf_ (current . _Just . workspace . tree . focusT . treeElements . _Just .
                                                                          focusL .
                                                                          _Left)
            s
            (\v -> wlcViewFocus v >> wlcViewBringToFront v)
     forOf_ (current . _Just . screen) s wlcOutputFocus

layoutScreen :: Screen i WLCViewPtr WLCOutputPtr -> IO ()
layoutScreen (Screen w sid res) = do
  wlcOutputSetMask sid (w ^. mask)
  layoutWorkspace res w

-- | Resize all views on the workspace according to the current layout
layoutWorkspace :: WLCSize -> Workspace i WLCViewPtr -> IO ()
layoutWorkspace size ws =
  layoutTree size
             (integrateTree (ws ^. tree))

layoutTree :: WLCSize -> Tree Layout WLCViewPtr -> IO ()
layoutTree screenSize mainTree =
  go (WLCGeometry (WLCOrigin 0 0)
                  screenSize)
     mainTree
  where go _ (Tree _ Nothing) = return ()
        go (WLCGeometry (WLCOrigin x y) size) (Tree l (Just z)) =
          let arrangement =
                getLayout l size z &
                each .
                _2 %~
                (\(WLCGeometry (WLCOrigin x' y') size') ->
                   WLCGeometry
                     (WLCOrigin (x + x')
                                (y + y'))
                     size')
          in mapM_ recurse arrangement
        recurse ((Left v),geometry) =
          wlcViewSetGeometry v 0 geometry >> -- TODO: figure that out
          wlcViewSetState v WlcBitMaximized True
        recurse ((Right t),geometry) =
          go geometry t


-- | insert the view into workspace that is focused on the output
insertViewInOutput :: Layout
                   -> a
                   -> WLCOutputPtr
                   -> StackSet i a WLCOutputPtr
                   -> StackSet i a WLCOutputPtr
insertViewInOutput l v output s =
  modifyWithOutput (insertUp l v)
                   output
                   s

cycleLayout :: Layout -> Layout
cycleLayout (Layout _ "Horizontal") = verticalLayout
cycleLayout (Layout _ "Vertical") = tabbedLayout
cycleLayout (Layout _ "Tabbed") = horizontalLayout
cycleLayout _ = horizontalLayout
