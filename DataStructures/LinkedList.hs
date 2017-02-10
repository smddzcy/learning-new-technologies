  module DataStructures.LinkedList
  ( empty
  , head'
  , tail'
  , last'
  , addFirst
  , addLast
  , removeFirst
  , removeLast
  , contains
  , fromList
  , toList
  ) where

import Data.Maybe()

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Show, Read, Eq)

empty :: LinkedList a
empty = Nil

singleton :: a -> LinkedList a
singleton a = Cons a Nil

head' :: LinkedList a -> Maybe a
head' (Cons a _) = Just a
head' Nil = Nothing

tail' :: LinkedList a -> Maybe (LinkedList a)
tail' (Cons _ t) = Just t
tail' Nil = Nothing

last' :: LinkedList a -> Maybe a
last' (Cons a Nil) = Just a
last' (Cons _ t) = last' t
last' Nil = Nothing

contains :: (Eq a) => LinkedList a -> a -> Bool
contains Nil _ = False
contains (Cons a t) x
  | a == x    = True
  | otherwise = contains t x

addFirst :: LinkedList a -> a -> LinkedList a
addFirst Nil a = singleton a
addFirst (Cons a t) x = Cons x $ addFirst t a

addLast :: LinkedList a -> a -> LinkedList a
addLast Nil a = singleton a
addLast (Cons a t) x = Cons a $ addLast t x

removeFirst :: LinkedList a -> Maybe (LinkedList a)
removeFirst (Cons _ t) = Just t
removeFirst Nil = Nothing

removeLast :: LinkedList a -> Maybe (LinkedList a)
removeLast (Cons _ Nil) = Just Nil
removeLast (Cons a t) = Cons a <$> removeLast t
removeLast Nil = Nothing

fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons a t) = a : toList t

instance Functor LinkedList where
  fmap _ Nil = Nil
  fmap f (Cons a t) = Cons (f a) $ fmap f t
