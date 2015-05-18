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
  Tree {_layout :: l
       ,_treeElements :: (Maybe (ListZipper (Either a (Tree l a))))}
  deriving (Show,Read,Eq,Ord)

data ListZipper a =
  ListZipper {_focusL :: !a
             ,_leftL :: [a]
             ,_rightL :: [a]}
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
  TreeZipper {_focusT :: Tree l a
             ,_parentsT :: [([Tree l a],l,[Tree l a])]}
  deriving (Show,Read,Eq,Ord)

instance (Pretty (Tree l a),Pretty l) => Pretty (TreeZipper l a) where
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
       (return (ListZipper (return f)
                           (map return ls)
                           (map return rs)))
