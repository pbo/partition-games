import Lotto
import Math.Combinat.Partitions
import qualified Data.Set as S
import Data.List (tails)

contraryPairsSet :: [Partition] -> S.Set (Partition, Partition)
contraryPairsSet xs = S.filter isContrary (uniquePairs (S.fromList xs))
  where uniquePairs s = S.fromDistinctAscList [(x,y) | (x:xt) <- tails (S.toList s), y <- xt]
        isContrary (x, y) = let (l, _, w) = tommyLotto x y in  signum (force x y) /= signum (w - l)

main :: IO ()
main = print $ contraryPairsSet xs
  where xs = allLottoPartitions n m
        (n, m) = (8, 4)
