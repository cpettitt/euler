module Util where

import Data.List (group)

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

-- | Given an ordered list, this equation returns a new ordered list with all
-- duplicates removed. This is similar to `nub`, but with the ordered
-- constraint, this equation runs in O(n) and works with infinite lists.
uniq :: Eq a => [a] -> [a]
uniq = map head . group

-- | Generates an infinite, lazy list of fibonacci numbers using the arguments
-- as the first two numbers in the sequence.
fibs :: Int -> Int -> [Int]
fibs n0 n1 = fibs'
    where
      fibs' = n0 : n1 : zipWith (+) fibs' (tail fibs')

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

divides :: Int -> Int -> Bool
x `divides` y = y `rem` x == 0
