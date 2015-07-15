import Lotto
import Math.Combinat.Partitions
import qualified Data.Set as S
import Data.List (tails)
import Control.Monad

uniquePairs :: [t] -> S.Set (t, t)
uniquePairs xs = S.fromDistinctAscList [(x,y) | (x:xt) <- tails xs, y <- xt]

resourceMatrix :: Partition -> Partition -> [([Int], [Int])]
resourceMatrix (Partition xs) (Partition ys) =
    if xs == xs' && ys == ys'
        then []
        else (xs', ys') : resourceMatrix (Partition xs') (Partition ys')
    where xs'  = resourceVec' xs ys
          ys'  = resourceVec' ys xs

resourceMatrixResult :: (Num a, Eq a) => [([a], [a])] -> a
resourceMatrixResult [] = 0
resourceMatrixResult ((xs, ys) : restM) =
    let s = signum (sum xs - sum ys) in
        case s of 0 -> resourceMatrixResult restM
                  _ -> s

main :: IO ()
main = do
    let (n, m) = (15, 5)
        xs = allLottoPartitions n m
    _ <- forM (S.toList (uniquePairs xs)) (\(x, y) -> do
        let (l, t, w) = tommyLotto x y
            isContrary = signum (force x y) /= signum (w - l)
        when isContrary $ do
            let rm = resourceMatrix x y
                isContrary' = signum (w - l) /= resourceMatrixResult rm
            putStrLn (if isContrary' then "!" else [])
            putStrLn $ show (fromPartition x) ++ " vs " ++ show (fromPartition y) ++ ":"
            putStrLn $ "\tWins: " ++ show w ++ ", Ties: " ++ show t ++ ", Losses: " ++ show l
            _ <- forM rm (\(x', y') -> putStrLn $ "\t" ++ show x' ++ " = " ++ show (sum x') ++ " vs " ++ show y' ++ " = " ++ show (sum y'))
            putStrLn "")
    putStrLn ""
