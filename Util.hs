module Util where

import Data.List (group, nub, subsequences)

import PrimeGen (primes)

groupsOf :: Int -> [a] -> [[a]]
groupsOf n = go
    where
      go xs | length xs < n = []
      go xs = take n xs : go (tail xs)

-- | Returns a list of all unique prime factors of `n`.
primeFactors :: Int -> [Int]
primeFactors 0 = []
primeFactors 1 = []
primeFactors x = d : primeFactors (x `div` d)
    where
      d = head $ filter (`divides` x) primes

-- | Unique factors (including trivial factors) for a given number.
factors :: Int -> [Int]
factors = nub . map product . subsequences . primeFactors

-- | Given an ordered list, this equation returns a new ordered list with all
-- duplicates removed. This is similar to `nub`, but with the ordered
-- constraint, this equation runs in O(n) and works with infinite lists.
uniq :: Eq a => [a] -> [a]
uniq = map head . group

-- | Generates an infinite, lazy list of fibonacci numbers using the arguments
-- as the first two numbers in the sequence.
fibs :: Integral a => a -> a -> [a]
fibs n0 n1 = fibs'
    where
      fibs' = n0 : n1 : zipWith (+) fibs' (tail fibs')

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

triangleNums :: [Int]
triangleNums = scanl1 (+) [1..]

rot45 :: [[Int]] -> [[Int]]
rot45 [] = []
rot45 xs = xs' `seq` go (w - 1) []
    where
      w   = length (head xs)
      h   = length xs
      wh  = w * h
      xs'
        | all ((== w) . length) xs = concat xs
        | otherwise = error "rot45: mixed dimensions in inner arrays"

      go :: Int -> [[Int]] -> [[Int]]
      go i as
        | i == wh          = reverse as
        | i >= w || i == 0 = go (i + w) (expand i (min (h - (i `div` w)) w) : as)
        | otherwise        = go (i - 1) (expand i (min (w - i) h)           : as)

      expand :: Int -> Int -> [Int]
      expand _ 0 = []
      expand i n = xs' !! i : expand (i + w + 1) (n - 1)

divides :: Int -> Int -> Bool
x `divides` y = y `rem` x == 0
