module CH6 where

import Prelude
import Data.Traversable (class Foldable, foldMap, foldl, foldr)

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty el xs) = "(" <> show el <> ", " <> show xs <> ")"

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty el1 xs1) (NonEmpty el2 xs2) = el1 == el2 && xs1 == xs2

instance semigroupNonEmpty :: Semigroup a => Semigroup (NonEmpty a) where
  append (NonEmpty el1 xs1) (NonEmpty el2 xs2) = NonEmpty (el1 <> el2) (xs1 <> xs2)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty el xs) = NonEmpty (f el) (f <$> xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr fn acc (NonEmpty el xs) = foldr fn acc xs
  foldl fn acc (NonEmpty el xs) = foldl fn acc xs
  foldMap fn (NonEmpty el xs) = foldMap fn xs

data Extended a = Finite a | Infinite

instance showExtended :: Show a => Show (Extended a) where
  show (Finite a) = show a
  show Infinite = "Infinite"

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite a) (Finite b) = a == b
  eq Infinite Infinite = true
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite a) (Finite b) | a < b = LT
                                | a > b = GT
                                | otherwise = EQ
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT

data OneMore f a = OneMore a (f a)
instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldr fn acc (OneMore el xs) = fn el (foldr fn acc xs)
  foldl fn acc (OneMore el xs) = fn (foldl fn acc xs) el
  foldMap fn (OneMore el xs) = fn el <> foldMap fn xs
