module DataStructures.Queue
  ( empty
  , offer
  , poll
  , peek
  ) where

import Data.Maybe()

data Queue a = Empty | Queue a (Queue a) deriving (Show, Eq)

empty :: Queue a
empty = Empty

poll :: Queue a -> Maybe (Queue a, a)
poll (Queue a xs) = Just (xs, a)
poll Empty = Nothing

offer :: a -> Queue a -> Queue a
offer a Empty = Queue a Empty
offer a (Queue b xs) = Queue b $ offer a xs

peek :: Queue a -> Maybe a
peek (Queue a _) = Just a
peek Empty = Nothing
