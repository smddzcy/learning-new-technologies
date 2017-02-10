module DataStructures.Stack
  ( empty
  , pop
  , push
  , peek
  ) where

import Data.Maybe()

data Stack a = Empty | Stack a (Stack a) deriving (Show, Eq)

empty :: Stack a
empty = Empty

pop :: Stack a -> Maybe (Stack a, a)
pop (Stack a xs) = Just (xs, a)
pop Empty = Nothing

push :: a -> Stack a -> Stack a
push a Empty = Stack a Empty
push a (Stack b xs) = Stack a $ push b xs

peek :: Stack a -> Maybe a
peek (Stack a _) = Just a
peek Empty = Nothing
