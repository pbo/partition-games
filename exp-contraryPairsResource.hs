import Lotto
import Math.Combinat.Partitions
import qualified Data.Set as S
import Data.List (tails)
import Control.Monad

uniquePairs :: [t] -> S.Set (t, t)
uniquePairs xs = S.fromDistinctAscList [(x,y) | (x:xt) <- tails xs, y <- xt]

main :: IO ()
main = do
    let (n, m) = (16, 4)
        xs = allLottoPartitions n m
    _ <- forM (S.toList (uniquePairs xs)) (\(x, y) -> do
        let (l, t, w) = tommyLotto x y
            isContrary = signum (force x y) /= signum (w - l)
        when isContrary $ do
            putStrLn $ show (fromPartition x) ++ " vs " ++ show (fromPartition y) ++ ":"
            putStrLn $ "\tWins: " ++ show w ++ ", Ties: " ++ show t ++ ", Losses: " ++ show l
            let r1 = resourceVec x y
                r2 = resourceVec y x
            putStrLn $ "\tR:    " ++ show r1 ++ " = " ++ show (sum r1) ++ " vs "
                                  ++ show r2 ++ " = " ++ show (sum r2)
            let r1' = resourceVec' r1 r2
                r2' = resourceVec' r2 r1
            putStrLn $ "\tR':   " ++ show r1' ++ " = " ++ show (sum r1') ++ " vs "
                                  ++ show r2' ++ " = " ++ show (sum r2')
            let r1'' = resourceVec' r1' r2'
                r2'' = resourceVec' r2' r1'
            putStrLn $ "\tR'':  " ++ show r1'' ++ " = " ++ show (sum r1'') ++ " vs "
                                  ++ show r2'' ++ " = " ++ show (sum r2'')
            let r1''' = resourceVec' r1'' r2''
                r2''' = resourceVec' r2'' r1''
            putStrLn $ "\tR''': " ++ show r1''' ++ " = " ++ show (sum r1''') ++ " vs "
                                  ++ show r2''' ++ " = " ++ show (sum r2''')
            putStrLn "")
    putStrLn ""
