module DataStructures.BinaryTree
  ( singleton
  , insert
  , contains
  , successor
  , getData
  , Tree(..) -- Import Tree type constructor with all of its value constructors
  ) where

-- () makes it insert only the constructors
import Data.Maybe()

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

leftChild :: Tree a -> Tree a
leftChild (Node _ left _) = left
leftChild Empty = Empty

rightChild :: Tree a -> Tree a
rightChild (Node _ _ right) = right
rightChild Empty = Empty

getData :: Tree a -> Maybe a
getData (Node a _ _) = Just a
getData Empty = Nothing

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node root left right)
  | x < root  = Node root (insert x left) right
  | x > root  = Node root left (insert x right)
  | otherwise = Node root left right

contains :: (Ord a) => a -> Tree a -> Bool
contains _ Empty = False
contains x (Node root left right)
  | x < root  = contains x left
  | x > root  = contains x right
  | otherwise = True

successor :: (Ord a) => Tree a -> Tree a
successor tree = case rightChild tree of
  Empty -> tree
  _     -> until (\x -> leftChild x == Empty) leftChild tree
