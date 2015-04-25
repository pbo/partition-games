import Lotto
import qualified Data.Set as S
import Data.List (tails)

main :: IO ()
main = print contraryPairsSet
  where
    contraryPairsSet = S.filter isContrary (uniquePairs (S.fromList allPartitions))
    uniquePairs s = S.fromDistinctAscList [(x,y) | (x:xt) <- tails (S.toList s), y <- xt]
    allPartitions = allLottoPartitions n m
    (n, m) = (8, 4)
    isContrary (xs, ys) =
      let (l, _, w) = tommyLotto xs ys
      in  signum (force xs ys) /= signum (w - l)
