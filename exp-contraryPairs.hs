import Lotto
import Math.Combinat.Partitions
import qualified Data.Set as S
import Data.List (tails)

uniquePairs :: [t] -> S.Set (t, t)
uniquePairs xs = S.fromDistinctAscList [(x,y) | (x:xt) <- tails xs, y <- xt]

contraryPairsSet :: [Partition] -> S.Set (Partition, Partition)
contraryPairsSet xs = S.filter isContrary (uniquePairs xs)
  where isContrary (x, y) = let (l, _, w) = tommyLotto x y in  signum (force x y) /= signum (w - l)

main :: IO ()
main = print $ contraryPairsSet xs
  where xs = allLottoPartitions n m
        (n, m) = (8, 4)