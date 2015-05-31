{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Tree
  (Tree(..)
  ,ListZipper(..)
  ,TreeZipper(..)
  ,integrate
  ,integrate'
  ,createZipper
  ,insertUp
  ,focusL
  ,leftL
  ,rightL
  ,focusT
  ,split
  ,moveDown
  ,moveUp
  ,moveViewUp
  ,parentsT
  ,layout
  ,treeElements
  ,integrateTree
  ) where

import Text.PrettyPrint.HughesPJClass
import Control.Lens

-- | l is an annotation at a branch
-- | a is the type of a leaf
data Tree l a =
  Tree {_layout :: !l
       ,_treeElements :: !(Maybe (ListZipper (Either a (Tree l a))))}
  deriving (Show,Read,Eq,Ord)

data ListZipper a =
  ListZipper {_focusL :: !a
             ,_leftL :: ![a]
             ,_rightL :: ![a]}
  deriving (Show,Read,Eq,Ord)

instance Pretty a => Pretty (ListZipper a) where
  pPrint (ListZipper f l r) =
    text "ListZipper" $$
    nest 2
         (text "focus =" <+>
          pPrint f $$
          text "left =" <+>
          pPrint l $$
          text "right =" <+>
          pPrint r)

makeLenses ''Tree
makeLenses ''ListZipper

data TreeZipper l a =
  TreeZipper {_focusT :: !(Tree l a)
             ,_parentsT :: ![([Either a (Tree l a)],l,[Either a (Tree l a)])]}
   deriving (Show,Read,Eq,Ord)

instance (Pretty (Tree l a),Pretty a,Pretty l) => Pretty (TreeZipper l a) where
  pPrint (TreeZipper f p) =
    text "TreeZipper" $$
    nest 2
         (text "focus =" <+>
          pPrint f $$
          text "parents =" <+>
          pPrint p)

instance (Pretty l,Pretty (ListZipper (Either a (Tree l a)))) => Pretty (Tree l a) where
  pPrint (Tree l el) = text "Tree" $$ nest 2 (text "layout =" <+> pPrint l $$ text "elements =" <+> pPrint el)

makeLenses ''TreeZipper

createZipper :: Tree a l -> TreeZipper a l
createZipper t = TreeZipper t []

insertUpList :: a -> Maybe (ListZipper a) -> (ListZipper a)
insertUpList a Nothing = ListZipper a [] []
insertUpList a (Just (ListZipper f l r)) = ListZipper a l (f:r)

-- if a leaf is focused split it up and use the supplied annotation
-- otherwise just move the current focus to the right and insert
insertUp :: l -> a -> TreeZipper l a -> TreeZipper l a
insertUp _ el z =
  z & focusT . treeElements %~
  (return .
   insertUpList (Left el))

integrate :: ListZipper a -> [a]
integrate (ListZipper x l r) = reverse l ++ x : r

integrate' :: Maybe (ListZipper a) -> [a]
integrate' = maybe [] integrate

integrateTree :: TreeZipper l a -> Tree l a
integrateTree (TreeZipper f []) = f
integrateTree (TreeZipper f ((ls,l,rs):xs)) =
  integrateTree $
  flip TreeZipper xs $
  Tree l
       (Just (ListZipper (Right f)
                         ls
                         rs))

split :: TreeZipper l a -> TreeZipper l a
split (TreeZipper (Tree l Nothing) p) = TreeZipper (Tree l Nothing) p
split (TreeZipper (Tree l (Just (ListZipper f ls rs))) ps) =
  TreeZipper
    (Tree l
          (Just (ListZipper (Right (Tree l (Just (ListZipper f [] []))))
                            ls
                            rs))) ps

moveDown :: TreeZipper l a -> TreeZipper l a
moveDown (TreeZipper (Tree l Nothing) ps) = TreeZipper (Tree l Nothing) ps
moveDown (TreeZipper (Tree l (Just (ListZipper (Left f) ls rs))) ps) = TreeZipper (Tree l (Just (ListZipper (Left f) ls rs))) ps
moveDown (TreeZipper (Tree l (Just (ListZipper (Right t) ls rs))) ps) = TreeZipper t ((ls,l,rs):ps)

moveUp :: TreeZipper l a -> TreeZipper l a
moveUp (TreeZipper t []) = TreeZipper t []
moveUp (TreeZipper t ((ls,l,rs):ps)) =
  TreeZipper
    (Tree l
          (Just (ListZipper (Right t)
                            ls
                            rs)))
    ps

moveViewUp :: TreeZipper l a -> TreeZipper l a
moveViewUp tz@(TreeZipper _ []) = tz
moveViewUp tz@(TreeZipper (Tree _ Nothing) _) = tz
moveViewUp (TreeZipper (Tree l (Just z)) ((ls,lp,rs):p)) =
  case removeFocused z of
    Just z' ->
      TreeZipper
        (Tree l (Just z'))
        ((z ^. focusL : ls,lp,rs) :
         p)
    Nothing ->
      TreeZipper
        (Tree lp
              (Just (ListZipper (z ^. focusL)
                                ls
                                rs)))
        p

removeFocused :: ListZipper a -> Maybe (ListZipper a)
removeFocused (ListZipper _ [] []) = Nothing
removeFocused (ListZipper _ (x:xs) rs) = Just $ ListZipper x xs rs
removeFocused (ListZipper _ [] (x:xs)) = Just $ ListZipper x [] xs
