import Data.Function (on)
import Data.List (foldl', group, sort)

{-------------------------------------------------------------------------------
 - Problems
 ------------------------------------------------------------------------------}

prob1 = sum $ filter (\x -> any (`divides` x) [3,5]) [1..999]

prob2 = sum $ filter even $ takeWhile (<= 4 * 1000 * 1000) (fibs 1 2)

prob3 = last $ uniq $ primeFactors 600851475143

prob4 = maximum $ filter (palindrome . show) [x * y | x <- [1..999], y <- [1..x]]

prob5 = foldr lcm 1 [2..20]

{-------------------------------------------------------------------------------
 - Helpers
 ------------------------------------------------------------------------------}

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

-- | Generates an infinite, lazy list of primes using the sieve of eratosthenes
primes :: [Int]
primes = 2 : go [3, 5 ..]
    where
      go (x:xs) = x : go (filter (not . (x `divides`)) xs)

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
