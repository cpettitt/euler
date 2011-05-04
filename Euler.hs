import Data.Char (digitToInt)
import Data.List (group)

main :: IO ()
main = mapM_ showAnswer
          [("prob1",   prob1)
          ,("prob2",   prob2)
          ,("prob3",   prob3)
          ,("prob4",   prob4)
          ,("prob5",   prob5)
          ,("prob6",   prob6)
          ,("prob7",   prob7)
          ,("prob8",   prob8)
          ,("prob9",   prob9)
          ]
    where
      showAnswer (name, f) = putStrLn $ name ++ " = " ++ show f


{-------------------------------------------------------------------------------
 - Problems
 ------------------------------------------------------------------------------}

prob1 :: Int
prob1 = sum $ filter (\x -> any (`divides` x) [3,5]) [1..999]

prob2 :: Int
prob2 = sum $ filter even $ takeWhile (<= 4 * 1000 * 1000) (fibs 1 2)

prob3 :: Int
prob3 = last $ uniq $ primeFactors 600851475143

prob4 :: Int
prob4 = maximum $ filter (palindrome . show) [x * y | x <- [1..999], y <- [1..x]]

prob5 :: Int
prob5 = foldr lcm 1 [2..20]

prob6 :: Int
prob6 = sum xs ^ 2 - sum (map (^2) xs)
    where
      xs = [1..100]

prob7 :: Int
prob7 = primes !! 10000

prob8 :: Int
prob8 = maximum $ map product $ groupsOf 5 $ map digitToInt (show n)
    where
      n = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450 :: Integer

prob9 :: Int
prob9 = head [ x * y * z
                | z <- [335..1000]
                , y <- [1..z-1]
                , x <- [1000 - z - y]
                , x < y
                , x ^ 2 + y ^ 2 == z ^ 2 ]


{-------------------------------------------------------------------------------
 - Helpers
 ------------------------------------------------------------------------------}

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
