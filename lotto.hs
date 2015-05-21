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

resource :: Partition -> Partition -> Int
resource (Partition xs) (Partition ys) = sum [heaviside (x - y) | x <- xs, y <- ys]
  where  heaviside x | x > 0 = 1
                     | otherwise = 0

eigenResource :: Partition -> Int
eigenResource xs = resource xs xs

maxEigenResource :: Int -> Int
maxEigenResource m = m * (m - 1) `div` 2

balance :: Partition -> Double
balance (Partition xs) = - fromIntegral sigma / fromIntegral n
  where sigma = sum $ zipWith (*) [m_2, (m_2-1) .. 1] diffs
        diffs = zipWith (-) (take m_2 xs) (reverse xs)
        m_2 = length xs `div` 2
        n = sum xs

{--------------------------------------------------------------------
  Interaction
--------------------------------------------------------------------}

interactionMatrix :: Partition -> Partition -> [Int]
interactionMatrix (Partition xs) (Partition ys) = [signum (x - y) | x <- xs, y <- ys]

classVSClass :: [Partition] -> [Partition] -> [Float]
classVSClass cx cy = [rel (sum [lotto px py | py <- cy]) | px <- cx]
	where lotto px py = if sum (interactionMatrix px py) < 0 then 0::Int else 1::Int
	      rel a = fromIntegral a / m
	      m = fromIntegral $ length cy