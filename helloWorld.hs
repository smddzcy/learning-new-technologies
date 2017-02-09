module Main where
-- Just sticking to the convention.
firstString :: String
firstString = "Hello, World!"
-- main = print firstString

-- A basic function declaration.
-- Left of "=>" declares the typeclasses.
-- Right part declares the input and output.
doubleIt :: (Num a) => a -> a
doubleIt a = a * 2

-- A much basic function declaration.
-- Output of ":t concatenate" = "concatenate :: [a] -> [a] -> [a]"
concatenate :: [a] -> [a] -> [a]
concatenate a b = a ++ b

-- Sorting

-- Quicksort
-- Pivot is the first element.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger  = quicksort [a | a <- xs, a > x]
    in  smaller ++ [x] ++ bigger

-- Mergesort
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort x =
    let (left, right) = splitAt (length x `div` 2) x
    in merge (mergesort left) (mergesort right)

f :: (Num a) => a -> a
f x = x + 10

g :: (Num a) => a -> a
g x = x * 2


main :: IO ()
main = print "Hey"
