module Data.Path where

import Prelude

import Data.Array.Partial (head, tail)
import Data.Traversable (class Foldable, foldl)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.MonadPlus (guard)
import Data.Array (concat, filter, length, (..))

infix 8 filter as <$?>

isEven :: Int -> Boolean
isEven num = (mod num 2) == 0

numOfEvens :: Array Int -> Int
numOfEvens arr = length $ filter isEven arr

squares :: forall f a. (Functor f, Semiring a) => f a -> f a
squares arr = (\x -> x * x) <$> arr

removeNegatives :: Array Int -> Array Int
removeNegatives arr = filter (\num -> num >= 0) arr


factors :: Int -> Array (Array Int)
factors n = do
  i <- 1..n
  j <- i..n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime num = factors num == [[1, num]]

cartesianProduct :: forall a b. (Bind a, Applicative a) => a b -> a b -> a (Array b)
cartesianProduct set1 set2 = do
  i <- set1
  j <- set2
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1..n
  j <- 1..n
  k <- 1..n
  guard $ i * i + j * j == k * k
  pure [i, j, k]

factorizations :: Int -> Array (Array Int)
factorizations 1 = [[1]]
factorizations n = do
  i <- 1..(n-1)
  guard $ mod n i == 0
  if i == 1
    then pure [1, n]
    else pure $ removeOnes $ concat $ (factorizations i) <> factorizations (n/i)
  where
    removeOnes :: Array Int -> Array Int
    removeOnes arr = filter (\x -> x /= 1) arr

_sum :: forall f a. (Foldable f, Semiring a) => f a -> a
_sum arr = foldl (\acc n -> acc + n) zero arr

_reverse :: forall a. Array a -> Array a
_reverse arr = foldl (\acc xs -> [xs] <> acc) [] arr
-- or:       = foldr (\xs acc -> acc <> [xs]) [] arr

allTrue :: forall t. (Foldable t) => t Boolean -> Boolean
allTrue = foldl (\acc xs -> acc && xs) true

predicateCount :: forall p. (p -> Boolean) -> Array p -> Int
predicateCount = predicateCount' 0
  where
    predicateCount' c p [] = c
    predicateCount' c p xs = predicateCount' (c + if p (unsafePartial head xs) then 1 else 0) p (unsafePartial tail xs)

main :: forall t. Eff ( "console" :: CONSOLE | t ) Unit
main = do
  logShow $ _sum [1,3,4]

