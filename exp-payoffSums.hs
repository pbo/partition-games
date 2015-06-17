{-# LANGUAGE OverloadedStrings #-}

import Lotto
import Utils
import Data.Csv
import Data.List.Ordered (sortOn)
import Math.Combinat.Partitions

-- Calculate `sum(L(a, B))` - sum of payoff functions in colonel Lotto game - for specified partition `a` and all partitions `b` in `B`.
payoffSum :: [Partition] -> Partition -> Int
payoffSum ps xs = sum $ map (force xs) ps

-- Calculate `sum(L(P, P))` matrix for all partitions `a` in P and all partitions `b` in P.
payoffSums :: [Partition] -> [Int]
payoffSums ps = map (payoffSum ps) ps

-- Calculate `sum(sign(L(a, B)))` - sum of payoff functions in colonel Lotto game - for specified partition `a` and all partitions `b` in `B`.
signumPayoffSum :: [Partition] -> Partition -> Int
signumPayoffSum ps xs = sum $ map (signum . force xs) ps

-- Calculate `sum(sign(L(P, P)))` matrix for all partitions `a` in P and all partitions `b` in P.
signumPayoffSums :: [Partition] -> [Int]
signumPayoffSums ps = map (signumPayoffSum ps) ps

main :: IO ()
main = saveCSV filename ["payoffSum"] $ map Only (signumPayoffSums ps)
  where
    filename = "output/payoff-sums-n=" ++ show n ++ "-m=" ++ show m ++ ".csv"
    ps = sortOn balance (allLottoPartitions n m)
    n = 36
    m = 6
