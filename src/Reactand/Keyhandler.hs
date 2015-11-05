{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Reactand.Keyhandler
  ( handleKey
  ) where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Reactand.Layout
import           Reactand.StackSet
import           Reactand.Tree
import           Reactand.Types
import           Text.XkbCommon hiding (layout)
import           Text.XkbCommon.KeysymList
import           WLC

handleKey :: Eq sid => Key -> StackSet String a sid -> (StackSet String a sid, [Command])
handleKey (Key WlcKeyStatePressed sym mods) =
  handleBindings defaultBindings sym mods
handleKey _ = (,[])

handleBindings :: [Binding a sid]
               -> Keysym
               -> Set.Set WLCModifier
               -> StackSet String a sid
               -> (StackSet String a sid,[Command])
handleBindings bindings sym mods =
  case getAlt $ mconcat $ map (Alt . matchBinding sym mods) bindings of
    Nothing -> (,[])
    Just f -> f

matchBinding
  :: Keysym
  -> Set.Set WLCModifier
  -> Binding a sid
  -> Maybe (StackSet String a sid -> (StackSet String a sid,[Command]))
matchBinding sym mods ((bindMods,bindSym) :=> f)
  | bindMods == mods && bindSym == sym = Just f
matchBinding _ _ _ = Nothing

defaultMod :: Set.Set WLCModifier
defaultMod = Set.fromList [WlcBitModAlt]

data Binding a sid =
  (Set.Set WLCModifier,Keysym) :=>
  (StackSet String a sid -> (StackSet String a sid,[Command]))

infixl 8 :=>

defaultBindings :: Eq sid => [Binding a sid]
defaultBindings =
  [(defaultMod,keysym_Return) :=> (,[Run "weston-terminal"])
  ,(defaultMod,keysym_space)  :=> (,[]) .
   (current . _Just . workspace . tree . focusT . layout %~ cycleLayout)
  ,(defaultMod,keysym_n)      :=> (,[]) . focusDown
  ,(defaultMod,keysym_r)      :=> (,[]) . focusUp] ++ catMaybes (workspaces 10)

workspaces :: Eq sid => Int -> [Maybe (Binding a sid)]
workspaces n = map (switchToWorkspace . (`mod` 10)) [1..n']
  where n' = if n > 10 then 10 else n

switchToWorkspace :: Eq sid => Int -> Maybe (Binding a sid)
switchToWorkspace n =
  fmap (\sym -> (defaultMod,sym) :=> ((,[]) . viewWorkspace n'))
       (keysymFromName n')
  where n' = show n
