{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)

data Tree t = Nil | Node t (Tree t) (Tree t) deriving (Show,Read)

create :: t -> Tree t
create a = Node a Nil Nil

insert :: t -> Tree t -> Tree t
insert a t1 = Node a t1 Nil

insertLeft :: t -> Tree t -> Tree t
insertLeft a Nil = Node a Nil Nil
insertLeft a (Node x t1 t2) = Node x (insertLeft a t1) t2

insertRight:: t -> Tree t -> Tree t
insertRight a Nil = Node a Nil Nil
insertRight a (Node x t1 t2) = Node x t1 (insertRight a t2)

leftTurn :: Tree t -> Tree t
leftTurn (Node x t1 Nil) = Node x t1 Nil
leftTurn (Node x t0 (Node y t1 t2)) = Node y (Node x t0 t1) t2

rightTurn :: Tree t -> Tree t
rightTurn (Node x Nil t1) = Node x Nil t1
rightTurn (Node x (Node y t0 t1) t2) = Node y t0 (Node x t1 t2)

map :: (a -> b) -> Tree a -> Tree b
map f Nil = Nil
map f (Node x t1 t2) = Node (f x) (map f t1) (map f t2)