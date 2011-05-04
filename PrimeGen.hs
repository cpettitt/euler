-- This prime number generator uses a lazy wheel sieve as described in:
--
--    Colun Runciman, "Lazy wheel sieves and sprials of primes",
--    <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.55.7096>

module PrimeGen (primes) where

data Wheel = Wheel Int [Int]

primes :: [Int]
primes = sieve wheels primes primeSquares

wheels :: [Wheel]
wheels = Wheel 1 [1] : zipWith nextSize wheels primes

sieve :: [Wheel] -> [Int] -> [Int] -> [Int]
sieve (Wheel s ns : ws) ps qs =
      [ n' | o <- s : [2 * s, 3 * s .. (head ps - 1) * s]
           , n <- ns
           , n' <- [ n + o ]
           , s <= 2 || noFactorIn ps qs n' ]
      ++ sieve ws (tail ps) (tail qs)
sieve _ _ _ = error $ "sieve: will never reach this"

nextSize :: Wheel -> Int -> Wheel
nextSize (Wheel s ns) p = Wheel (s * p) [ n' | o  <- [0, s .. (p - 1) * s]
                                             , n  <- ns
                                             , n' <- [n + o]
                                             , n' `mod` p > 0 ]

noFactorIn :: [Int] -> [Int] -> Int -> Bool
noFactorIn (p:ps) (q:qs) x = q > x || x `mod` p > 0 && noFactorIn ps qs x
noFactorIn _ _ _ = error $ "noFactorIn: will never reach this"

primeSquares :: [Int]
primeSquares = map (\x -> x * x) primes
