module Main where

import           Control.Arrow
import           Data.List
import           System.Random (randomRIO)

-- Find the last element of a list
last1 :: [a] -> a
last1 xs = xs !! (length xs - 1)

-- Find the last but one element of a list
secondLast :: [a] -> Maybe a
secondLast []     = Nothing
secondLast [x, _] = Just x
secondLast (_:xs) = secondLast xs

-- Find the K'th element of a list
kth :: [a] -> Integer -> Maybe a
kth [] _     = Nothing
kth (x:_) 0  = Just x
kth (_:xs) n = kth xs (n - 1)

-- Find the number of elements of a list
length' :: [a] -> Integer
length' []     = 0
length' (_:xs) = 1 + length' xs

-- Reverse a list
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Find out whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse' xs

-- Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten (Elem a)  = [a]
flatten (List xs) = concatMap flatten xs

-- Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = x : compress (dropWhile (== x) xs)

-- Pack consecutive duplicates of list elements into sublists
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:packed) : pack remaining
    where (packed, remaining) = span (== x) xs

-- Run-length encoding of a list
encode :: Eq a => [a] -> [(Int, a)]
encode xs = (length &&& head) <$> pack xs

-- Modified run-length encoding
encodeModified :: Eq a => [a] -> [Either a (Int, a)]
encodeModified xs = encodeHelper <$> encode xs
    where encodeHelper (n, x) | n > 1     = Right (n, x)
                              | otherwise = Left x

-- Decode a run-length encoded list
decode :: Eq a => [Either a (Int, a)] -> [a]
decode []                  = []
decode (Right (n, x) : xs) = replicate n x ++ decode xs
decode (Left x : xs)       = x : decode xs

-- Run-length encoding of a list (direct solution)
encodeDirect :: Eq a => [a] -> [(Int, a)]
encodeDirect [] = []
encodeDirect (x:xs) = (length packed, x) : encodeDirect remaining
    where (packed, remaining) = span (== x) (x:xs)

-- Duplicate the elements of a list a given number of times
duplicate :: [a] -> Int -> [a]
duplicate xs n = foldl (\acc x -> acc ++ replicate n x) [] xs

-- Drop every N'th element from a list
dropEveryNth :: [a] -> Int -> [a]
dropEveryNth xs n = dropEveryNth' xs n n

dropEveryNth' :: [a] -> Int -> Int -> [a]
dropEveryNth' [] _ _       = []
dropEveryNth' (_:xs) 1 def = dropEveryNth' xs def def
dropEveryNth' (x:xs) n def = x : dropEveryNth' xs (n - 1) def

-- Split a list into two parts; the length of the first part is given
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs) n = (x : fst rec, snd rec)
                 where rec = split xs (n - 1)

-- Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice [] _ _     = []
slice (x:_) 1 1  = [x]
slice (x:xs) 1 e = x : slice xs 1 (e - 1)
slice (_:xs) s e = slice xs (s - 1) (e - 1)

-- Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs n = snd' ++ fst' where (fst', snd') = split xs n

-- Remove the K'th element from a list
removeAt :: [a] -> Int -> ([a], Maybe a)
removeAt [] _ = ([], Nothing)
removeAt (x:xs) 1 = (xs, Just x)
removeAt (x:xs) n = (x : first', second') where (first', second') = removeAt xs (n - 1)

-- Insert an element at a given position into a list
insertAt :: [a] -> Int -> a -> [a]
insertAt [] _ x      = [x]
insertAt xs 1 x      = x : xs
insertAt (x:xs) n x' = x : insertAt xs (n - 1) x'

-- Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range s e = [s..e]

randomPick :: [a] -> IO a
randomPick xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- Extract a given number of randomly selected elements from a list
randomSelect :: [a] -> Int -> [IO a]
randomSelect xs n | n > 1     = randomPick xs : randomSelect xs (n - 1)
                  | otherwise = return $ randomPick xs

-- Draw N random numbers from the set 1..M
randomSelectLotto :: Int -> (Int, Int) -> [IO Int]
randomSelectLotto n (s, e) = randomSelect (range s e) n

-- Generate the combinations of K distinct objects chosen from the N elements of a list
combinations :: [a] -> Int -> [[a]]
combinations [] _ = []
combinations xs 1 = (: []) <$> xs
combinations (x:xs) n | length (x:xs) == n = [x:xs]
                      | otherwise = combinations xs n ++ (([x] ++) <$> combinations xs (n - 1))

-- Sorting a list of lists according to length of sublists
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = left' ++ [x] ++ right'
               where left' = lsort $ filter (\x' -> length x' <= length x) xs
                     right' = lsort $ filter (\x' -> length x' > length x) xs

-- Determine whether a given integer number is prime
factors :: (Integral a) => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: (Integral a) => a -> Bool
isPrime n = factors n == [1, n]

-- Determine the prime factors of a given positive integer
primeFactors :: Int -> [Int]
primeFactors n = concatMap (\x -> x : filter (== x) (primeFactors (n `div` x))) uniqPrimeFactors
                 where uniqPrimeFactors = filter isPrime $ factors n

-- Determine the prime factors of a given positive integer (2)
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult xs = (head &&& length) <$> pack (primeFactors xs)

-- A list of prime numbers
primeNumbers :: [Integer] -> [Integer]
primeNumbers = filter isPrime

-- Goldbach's conjecture
goldbach :: Integer -> [(Integer, Integer)]
goldbach n = [(x, y) | let facs = primeNumbers [2..n], x <- facs, y <- facs, x + y == n]

-- Determine the greatest common divisor of two positive integer numbers
gcd' :: (Ord a, Num a) => a -> a -> a
gcd' a 0 = a
gcd' 0 b = b
gcd' a b | a > b     = gcd' (a-b) b
         | otherwise = gcd' a (b-a)

-- Determine whether two positive integer numbers are coprime
coprime :: Int -> Int -> Bool
coprime x y = primeFactorsX \\ primeFactors y == primeFactorsX
              where primeFactorsX = primeFactors x

-- Logic functions
and' :: Bool -> Bool -> Bool
and' x y = x && y

or' :: Bool -> Bool -> Bool
or' x y = x || y

nand' :: Bool -> Bool -> Bool
nand' x y = not $ and' x y

nor' :: Bool -> Bool -> Bool
nor' x y = not $ or' x y

xor' :: Bool -> Bool -> Bool
xor' x y = (x || y) && not (x && y)

impl' :: Bool -> Bool -> Bool
impl' x = or' (not x)

equ' :: Bool -> Bool -> Bool
equ' x y = not $ xor' x y

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table fn = map (\(a,b) -> (a,b,fn a b)) [(True, True), (True, False), (False, True), (False, False)]

-- Gray code
insertAtEveryPos :: String -> Char -> [String]
insertAtEveryPos str c = [x | i <- [1..length str], let x = insertAt str i c]

gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = concatMap (\x -> concat $ insertAtEveryPos x '0' : [insertAtEveryPos x '1']) (gray (n - 1))

main :: IO ()
main = print $ table (\a b -> (and' a (or' a b)))
-- main = sequence_ $ (print . unsafePerformIO) <$> randomSelect [1,2,3,4,5] 3
