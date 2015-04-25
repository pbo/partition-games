module Lotto where

import Math.Combinat.Partitions
import Data.List (permutations)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

allLottoPartitions :: Int -> Int -> [Partition]
allLottoPartitions n m = map padZeros (partitions' (n, m) n)
  where padZeros (Partition xs) = Partition $ xs ++ replicate (subtract (length xs) m) 0

countLottoPartitions :: Int -> Int -> Integer
countLottoPartitions n m = countPartitions' (n, m) n

-- Returns the result of Blotto game of the first partition against the second
-- partition.
blotto :: (Num a) => [a] -> [a] -> a
blotto xs ys = signum $ sum $ zipWith (\x y -> signum (x - y)) xs ys

-- Returns number of losses, ties and wins in exhaustive Tommy-Lotto game of the
-- first partition against all permutations of the second partition.
tommyLotto :: Partition -> Partition -> (Int, Int, Int)
tommyLotto (Partition xs) (Partition ys) =
  foldl addSingleResult (0, 0, 0) (map (blotto xs) (permutations ys))
    where addSingleResult (l, t, w) r = case r of -1 -> (l + 1, t,     w    )
                                                  0  -> (l,     t + 1, w    )
                                                  1  -> (l,     t,     w + 1)
                                                  _  -> (l, t, w)

{-------------------------------------------------------------------------------
  Metrics
-------------------------------------------------------------------------------}

force :: Partition -> Partition -> Int
force (Partition xs) (Partition ys) = sum [signum (x - y) | x <- xs, y <- ys]

eigenforce :: Partition -> Int
eigenforce (Partition xs) = sum [heaviside (x - y) | x <- xs, y <- xs]
  where  heaviside n | n <= 0 = 0
                     | otherwise = 1
--eigenforce' (Partition xs) = sum [1 | x <- xs, y <- xs, x == y]

maxEigenforce :: Int -> Int
maxEigenforce n = n * (n - 1) `div` 2

balance :: Partition -> Double
balance (Partition xs) = - fromIntegral sigma / fromIntegral b
  where sigma = sum $ zipWith (*) [n_2,(n_2-1)..1] diffs
        diffs = zipWith (-) (take n_2 xs) (reverse xs)
        n_2 = length xs `div` 2
        b = sum xs

{--------------------------------------------------------------------
  Interaction
--------------------------------------------------------------------}

interactionMatrix :: Partition -> Partition -> [Int]
interactionMatrix (Partition xs) (Partition ys) = [signum (x - y) | x <- xs, y <- ys]

classVSClass :: [Partition] -> [Partition] -> [Float]
classVSClass cx cy = [rel (sum [lotto px py | py <- cy]) | px <- cx]
	where lotto px py = if sum (interactionMatrix px py) < 0 then 0::Int else 1::Int
	      rel a = fromIntegral a / n
	      n = fromIntegral $ length cy