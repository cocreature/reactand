{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Reactand.StackSet
  ( TreeZipper(..)
  , StackSet(..)
  , Workspace(..)
  , Screen(..)
  , deleteFromStackSet
  , viewWorkspace
  , focusUp
  , focusDown
  , changeResolution
  , swapUp
  , swapDown
  , modify
  , modifyWithOutput
  , createOutput
  , removeOutput
  , nextOutput
  , prevOutput
  , delete'
  , workspace
  , resolution
  , current
  , visible
  , hidden
  , screen
  , tag
  , mask
  , tree
  ) where

import Control.Lens
import Data.Function
import Data.List
import Data.Maybe
import Foreign.C.Types
import Text.PrettyPrint.HughesPJClass
import WLC

import Reactand.LayoutType
import Reactand.Tree

delete' :: Eq a => a -> [a] -> ([a],Bool)
delete' a xs =
  case break (== a) xs of
    (_,[]) -> (xs,False)
    (us,_:vs) -> (us ++ vs,True)

data StackSet i a sid =
  StackSet {_current :: !(Maybe (Screen i a sid))
           ,_visible :: ![Screen i a sid]
           ,_hidden :: ![Workspace i a]}
  deriving (Show,Eq)

instance (Pretty (Screen i a sid),Pretty (Workspace i a)) => Pretty (StackSet i a sid) where
  pPrint (StackSet c v h) =
    text "StackSet" $$
    nest 2
         (text "current =" <+>
          pPrint c $$
          text "visible =" <+>
          pPrint v $$
          text "hidden =" <+>
          pPrint h)

data Workspace i a =
  Workspace {_tag :: !i
            ,_mask :: !CUInt
            ,_tree :: !(TreeZipper Layout a)}
  deriving (Show,Eq)

instance (Pretty i,Pretty (TreeZipper Layout a)) => Pretty (Workspace i a) where
  pPrint (Workspace i m t) =
    text "Workspace" $$
    nest 2
         (text "tag =" <+>
          pPrint i $$
          text "mask =" <+>
          pPrintShow m $$
          text "tree = " <+>
          pPrint t)

pPrintShow :: Show a => a -> Doc
pPrintShow = text . show

data Screen i a sid =
  Screen {_workspace :: !(Workspace i a)
         ,_screen :: !sid
         ,_resolution :: !WLCSize}
  deriving (Show,Eq)

instance (Pretty (Workspace i a),Pretty sid) => Pretty (Screen i a sid) where
  pPrint (Screen w sid size) =
    text "Screen" $$
    nest 2 (pPrint w $$ pPrint sid $$ pPrintShow size)

makeLenses ''StackSet
makeLenses ''Workspace
makeLenses ''Screen

deleteEmpty :: Eq a => TreeZipper l a -> TreeZipper l a
deleteEmpty (TreeZipper t []) = (TreeZipper t [])
deleteEmpty tz@(TreeZipper (Tree _ (Just _)) _) = tz
deleteEmpty (TreeZipper _ (([],l,[]):p)) = TreeZipper (Tree l Nothing) p
deleteEmpty (TreeZipper _ ((x:xs,l,rs):p)) = TreeZipper (Tree l (Just (ListZipper x xs rs))) p
deleteEmpty (TreeZipper _ (([],l,x:xs):p)) = TreeZipper (Tree l (Just (ListZipper x [] xs))) p

deleteFromStackSet :: Eq a => a -> StackSet i a sid -> StackSet i a sid
deleteFromStackSet v s = s & current . _Just . workspace %~ deleteFromWorkspace v
                           & visible . mapped . workspace %~ deleteFromWorkspace v
                           & hidden . mapped %~ deleteFromWorkspace v

deleteFromWorkspace :: Eq a => a -> Workspace i a -> Workspace i a
deleteFromWorkspace v ws = ws & tree %~ deleteFromTreeZipper v

deleteFromTreeZipper :: Eq a => a -> TreeZipper l a -> TreeZipper l a
deleteFromTreeZipper v tz = deleteEmpty $ tz & focusT %~ deleteFromTree v
                                             & parentsT . mapped . _1_3 . mapped . _Right %~ deleteFromTree v

_1_3 :: Traversal (a,c,a) (b,c,b) a b
_1_3 f (a,b,c) = (\x y -> (x,b,y)) <$> f a <*> f c

deleteFromTree :: Eq a => a -> Tree l a -> Tree l a
deleteFromTree v t = t & treeElements %~ (deleteFromListZipper v =<<)

deleteFromListZipper :: Eq a => a -> ListZipper (Either a (Tree l a)) -> Maybe (ListZipper (Either a (Tree l a)))
deleteFromListZipper v (ListZipper (Left v') [] [])
  | v == v' = Nothing
  | otherwise = return (ListZipper (Left v') [] [])
deleteFromListZipper v (ListZipper (Left v') (l:ls) [])
  | v == v' = return (ListZipper l ls [])
  | otherwise = return (ListZipper (Left v') (deleteFromList v ls) [])
deleteFromListZipper v (ListZipper (Left v') ls (r:rs))
  | v == v' = return (ListZipper r ls rs)
  | otherwise = return (ListZipper (Left v') (deleteFromList v ls) (deleteFromList v rs))
deleteFromListZipper v (ListZipper (Right t) ls rs) =
  return (ListZipper (Right (deleteFromTree v t))
                     (deleteFromList v ls)
                     (deleteFromList v rs))

deleteFromList :: Eq a => a -> [Either a (Tree l a)] -> [Either a (Tree l a)]
deleteFromList v = mapMaybe f
  where
    f (Left v')
       | v' == v = Nothing
       | otherwise = return (Left v')
    f (Right t) = return (Right (deleteFromTree v t))


viewWorkspace :: (Eq i,Eq sid)
              => i -> StackSet i a sid -> StackSet i a sid
viewWorkspace _ s@(StackSet Nothing _ _) = s
viewWorkspace i s@(StackSet (Just currentScreen) visible' _)
  | i == currentScreen ^. workspace . tag = s
  | Just x <-
     find (\x -> i == x ^. workspace . tag)
          (s ^. visible) =
    s & current .~ Just x
      & visible .~ (currentScreen :
                    deleteBy (equating (view screen)) x visible')
  | Just x <-
     find (\x -> i == x ^. tag)
          (s ^. hidden) =
    s & current .~ Just (currentScreen & workspace .~ x)
      & hidden .~ (currentScreen ^. workspace :
                   deleteBy (equating (view tag))
                          x
                          (s ^. hidden))
  | otherwise = s

equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating = on (==)

createOutput :: sid -> WLCSize -> StackSet i a sid -> StackSet i a sid
createOutput _ _ (StackSet _ _ []) = error "No more workspaces available"
createOutput sid res s@(StackSet Nothing _ (x:xs)) =
  s & current .~ Just (Screen x sid res)
    & hidden .~ xs
createOutput sid res s@(StackSet _ visible' (x:xs)) =
  s & visible .~ (Screen x sid res) : visible'
    & hidden .~ xs

removeOutput :: Eq sid => sid -> StackSet i a sid -> StackSet i a sid
removeOutput sid s =
  StackSet current' visible' (hidden'' ++ hidden' ++ (s ^. hidden))
  where (visible',hidden') =
          deleteBySid sid (s ^. visible)
        (current',hidden'') =
          case (s ^. current) of
            Nothing -> (Nothing,[])
            Just screen'
              | screen' ^. screen == sid ->
                (Nothing,return $ screen' ^. workspace)
              | otherwise -> (Just screen',[])

deleteBySid :: Eq sid => sid -> [Screen i a sid] -> ([Screen i a sid],[Workspace i a])
deleteBySid sid screens =
  break ((== sid) .
         (view screen))
        screens &
  _2 . traverse %~
  (view workspace)

withOutput :: (TreeZipper Layout a -> b) -> Screen i a sid -> b
withOutput f s = f $ s ^. workspace . tree

modify :: (TreeZipper Layout a -> TreeZipper Layout a)
       -> StackSet i a sid
       -> StackSet i a sid
modify f s = s & current . _Just . workspace . tree %~ f

modifyWithOutput :: Eq sid
                 => (TreeZipper Layout a -> TreeZipper Layout a)
                 -> sid
                 -> StackSet i a sid
                 -> StackSet i a sid
modifyWithOutput f sid s =
  s & current . _Just %~
      (\cur -> modifyOutput f
                            sid
                            cur)
    & visible %~map (modifyOutput f sid)

modifyOutput :: Eq sid
             => (TreeZipper Layout a -> TreeZipper Layout a)
             -> sid
             -> Screen i a sid
             -> Screen i a sid
modifyOutput f sid s
  | sid == s ^. screen =
    s & workspace . tree .~
    withOutput f s
  | otherwise = s

modify' :: (ListZipper (Either a (Tree Layout a)) -> ListZipper (Either a (Tree Layout a)))
        -> StackSet i a sid
        -> StackSet i a sid
modify' f = modify (\tz -> tz & focusT . treeElements . _Just %~ f)

focusUp :: StackSet i a sid -> StackSet i a sid
focusUp = modify' focusUp'

focusDown :: StackSet i a sid -> StackSet i a sid
focusDown = modify' focusDown'

swapUp :: StackSet i a sid -> StackSet i a sid
swapUp    = modify' swapUp'

swapDown :: StackSet i a sid -> StackSet i a sid
swapDown  = modify' (reverseStack . swapUp' . reverseStack)

swapUp' :: ListZipper a -> ListZipper a
swapUp'  (ListZipper t (l:ls) rs) = ListZipper t ls (l:rs)
swapUp'  (ListZipper t []     rs) = ListZipper t (reverse rs) []

focusUp', focusDown' :: ListZipper a -> ListZipper a
focusUp' (ListZipper t (l:ls) rs) = ListZipper l ls (t:rs)
focusUp' (ListZipper t []     rs) = ListZipper x xs [] where (x:xs) = reverse (t:rs)
focusDown' = reverseStack . focusUp' . reverseStack

reverseStack :: ListZipper a -> ListZipper a
reverseStack (ListZipper t ls rs) = ListZipper t rs ls

nextOutput :: StackSet i a sid -> StackSet i a sid
nextOutput (StackSet c [] h) = (StackSet c [] h)
nextOutput (StackSet Nothing (x:xs) h) = (StackSet (Just x) xs h)
nextOutput (StackSet (Just s) (x:xs) h) = (StackSet (Just x) (xs++ [s]) h)

prevOutput :: StackSet i a sid -> StackSet i a sid
prevOutput (StackSet c [] h) = (StackSet c [] h)
prevOutput (StackSet Nothing xs h) = (StackSet (Just (last xs)) (init xs) h)
prevOutput (StackSet (Just s) xs h) = (StackSet (Just (last xs)) (s: init xs) h)

changeResolution' :: Eq sid => sid -> WLCSize -> Screen i a sid -> Screen i a sid
changeResolution' sid res s
  | sid == s ^. screen = s & resolution .~ res
  | otherwise = s

changeResolution :: Eq sid => sid -> WLCSize -> StackSet i a sid -> StackSet i a sid
changeResolution sid res s =
  s &
  current . _Just %~ changeResolution' sid res &
  visible . mapped %~ changeResolution' sid res
