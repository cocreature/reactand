{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module StackSet
  (Stack(..)
  ,StackSet(..)
  ,Workspace(..)
  ,Screen(..)
  ,insertUp
  ,modify
  ,viewWorkspace
  ,with
  ,createOutput
  ) where

import Data.List
import Foreign.C.Types

data Stack a =
  Stack {focus :: !a
        ,up :: [a]
        ,down :: [a]}
  deriving (Show,Read,Eq,Functor,Foldable)

data StackSet i l a sid =
  StackSet {current :: !(Maybe (Screen i l a sid))
           ,visible :: [Screen i l a sid]
           ,hidden :: [Workspace i l a]}
  deriving (Show,Read,Eq)

data Workspace i l a =
  Workspace {tag :: !i
            ,layout :: l
            ,mask :: !CUInt
            ,stack :: Maybe (Stack a)}
  deriving (Show,Read,Eq)

data Screen i l a sid =
  Screen {workspace :: !(Workspace i l a)
         ,screen :: !sid}
  deriving (Show,Read,Eq)

deleteFromWorkspace :: a -> Workspace i l a -> (Workspace i l a,Bool)
deleteFromWorkspace view w@(Workspace _ _ _ Nothing) = (w,False)
--deleteFromWorkspace view w@(Workspace _ _ _ (Just stack)) = _

deleteFromStack :: a -> Stack a -> (Maybe (Stack a),Bool)
deleteFromStack a (Stack focus [] down) =
  deleteFromStack a
                  (Stack focus (reverse down) [])
{-deleteFromStack a (Stack focus (x:xs) down)
  | a == xs = (Stack x xs down,True)
  | (up,True) <- delete' a (x:xs) = (Stack focus up down,True)
  | (down',True) <- delete' a down = (Stack focus (x:xs) down',True)
  | otherwise = (Stack focus (x:xs) down,False)-}

delete' :: Eq a => a -> [a] -> ([a],Bool)
delete' a xs =
  case break (== a) xs of
    (us,[]) -> (xs,False)
    (us,v:vs) -> (us ++ vs,True)

viewWorkspace :: (Eq i,Eq sid)
              => i -> StackSet i l a sid -> StackSet i l a sid
viewWorkspace _ s@(StackSet Nothing _ _) = s
viewWorkspace i s@(StackSet (Just currentScreen) visible' _)
  | i ==
      tag (workspace currentScreen) = s
  | Just x <-
     find ((i ==) . tag . workspace)
          (visible s) =
    s {current = Just x
      ,visible =
         currentScreen :
         deleteBy (equating screen) x visible'}
  | Just x <-
     find ((i ==) . tag)
          (hidden s) =
    s {current =
         Just (currentScreen {workspace = x})
      ,hidden =
         workspace currentScreen :
         deleteBy (equating tag)
                  x
                  (hidden s)}
  | otherwise = s

createOutput :: sid -> StackSet i l a sid -> StackSet i l a sid
createOutput _ (StackSet _ _ []) = error "No more workspaces available"
createOutput sid s@(StackSet Nothing _ (x:xs)) =
  s {current = Just (Screen x sid)
    ,hidden = xs}
createOutput sid s@(StackSet _ visible' (x:xs)) =
  s {visible =
       (Screen x sid) :
       visible'
    ,hidden = xs}

equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating f x y = f x == f y

insertUp :: a -> Stack a -> Stack a
insertUp a stack =
  stack {down = focus stack : down stack
        ,focus = a}

modify :: (Eq sid)
       => Maybe (Stack a)
       -> (Stack a -> (Maybe (Stack a)))
       -> sid
       -> StackSet i l a sid
       -> StackSet i l a sid
modify d f sid (s@StackSet{current = Nothing,.. }) = modifyVisible d f sid s
modify d f sid (s@StackSet{current = Just screen',..})
  | screen screen' == sid =
    s {current =
         Just (screen' {workspace =
                          (workspace screen') {stack =
                                                 maybe d f (stack (workspace screen'))}})}
  | otherwise = modifyVisible d f sid s

modifyVisible :: (Eq sid)
              => Maybe (Stack a)
              -> (Stack a -> (Maybe (Stack a)))
              -> sid
              -> StackSet i l a sid
              -> StackSet i l a sid
modifyVisible d f sid s =
  s {visible =
       map (\screen' ->
              if screen screen' == sid
                 then screen' {workspace =
                                 (workspace screen') {stack =
                                                        maybe d f (stack (workspace screen'))}}
                 else screen')
           (visible s)}

with :: b -> (Stack a -> b) -> StackSet i l a sid -> b
with d _ (StackSet {current = Nothing,..}) = d
with d f (StackSet{current = Just currentScreen,..}) =
  (maybe d f .
   stack . workspace) currentScreen
