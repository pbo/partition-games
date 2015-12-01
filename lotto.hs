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

uniform' :: Int -> Int -> [Int]
uniform' n m = zipWith (+) (replicate m q) (replicate r 1 ++ repeat 0)
  where (q, r) = n `quotRem` m

uniform :: Int -> Int -> Partition
uniform n m = toPartition (uniform' n m)

-- Returns the result of Blotto game of the first partition against the second
-- partition.
blotto :: (Num a) => [a] -> [a] -> a
blotto xs ys = signum $ sum $ zipWith (\x y -> signum (x - y)) xs ys

hB :: (Integral a1, Fractional a) => [a1] -> [a1] -> a
hB xs ys = fromIntegral (sum (zipWith (\x y -> signum (x - y)) xs ys)) / fromIntegral m
  where m = length xs

hL :: Fractional a => Partition -> Partition -> a
hL (Partition xs) (Partition ys) = fromIntegral (sum [signum (x - y) | x <- xs, y <- ys]) / fromIntegral (m * m)
  where m = length xs

hL' :: Fractional a => Partition -> Partition -> a
hL' (Partition xs) (Partition ys) = sum (map (hB xs) theta) / fromIntegral n
  where theta = permutations ys
        n = length theta

hL'' :: Fractional a => Partition -> Partition -> a
hL'' (Partition xs) (Partition ys) = sum (map (signum . hB xs) theta) / fromIntegral n
  where theta = permutations ys
        n = length theta

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

heaviside :: (Ord a, Num a1, Num a) => a -> a1
heaviside x | x > 0 = 1
            | otherwise = 0

force :: Partition -> Partition -> Int
force (Partition xs) (Partition ys) = sum [signum (x - y) | x <- xs, y <- ys]

force' :: Num a => [a] -> [a] -> a
force' xs ys = sum [signum (x - y) | x <- xs, y <- ys]

resource :: Partition -> Partition -> Int
resource (Partition xs) (Partition ys) = resource' xs ys

resource' :: (Ord a1, Num a1, Num a) => [a1] -> [a1] -> a
resource' xs ys = sum [heaviside (x - y) | x <- xs, y <- ys]

resourceVec :: Num t => Partition -> Partition -> [t]
resourceVec (Partition xs) (Partition ys) = resourceVec' xs ys

resourceVec' :: (Ord a, Num a, Num t) => [a] -> [a] -> [t]
resourceVec' xs ys = [sum [heaviside (x - y) | y <- ys] | x <- xs]

eigenResource :: Partition -> Int
eigenResource xs = resource xs xs

eigenResource' :: (Ord a, Floating a, Fractional a, Num a) => Partition -> a
eigenResource' (Partition xs) = resource'' (map fromIntegral xs) avgPartition
  where
    n = sum xs
    m = length xs
    avgPartition = map (\i -> (fromIntegral n / fromIntegral m) * log (fromIntegral m / fromIntegral i)) [1, 2 .. m]
    resource'' as bs = sum [max 0 (x - y) | x <- as, y <- bs]

maxEigenResource :: Int -> Int
maxEigenResource m = m * (m - 1) `div` 2

balance :: Partition -> Double
balance (Partition xs) = - fromIntegral sigma / fromIntegral n
  where sigma = sum $ zipWith (*) [m_2, (m_2-1) .. 1] diffs
        diffs = zipWith (-) (take m_2 xs) (reverse xs)
        m_2 = length xs `div` 2
        n = sum xs

distance :: Partition -> Partition -> Int
distance (Partition xs) (Partition ys) = sum (map abs (zipWith (-) xs ys)) `div` 2

distanceMax :: Partition -> Partition -> Int
distanceMax (Partition xs) (Partition ys) = maximum (map abs (zipWith (-) xs ys))

diameter :: (Applicative t, Foldable t) => t Partition -> Int
diameter ps = maximum $ distance <$> ps <*> ps

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