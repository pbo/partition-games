import Lotto
import Utils
import Data.Foldable (foldMap)
import Data.List (permutations)
import Math.Combinat.Partitions

winnigFields :: [Int] -> [Int] -> Int
winnigFields xs ys = sum $ zipWith (\x y -> notloss (x - y)) xs ys
    where notloss x | x >= 0 = 1
                    | otherwise = 0

score :: [Int] -> [Int] -> Int
score xs ys = sum $ zipWith (\x y -> notloss (x - y)) xs ys
    where notloss x | x >= 0 = 1
                    | otherwise = -1

mean :: [(Int, Int)] -> Double
mean distribution = fromIntegral s / fromIntegral count
    where s = sum $ map (uncurry (*)) distribution
          count = sum $ map snd distribution

main :: IO ()
main = do
    let (n, m) = (36, 6)
        nPWSPerField = ceiling (fromIntegral (2*n + m) / fromIntegral m :: Double)
        testeeComposition = replicate m nPWSPerField
        isEquilibrium (Partition (x:xs)) = x <= 2 * nPWSPerField + 2
        rP = filter isEquilibrium (allLottoPartitions n m)
        rC = foldMap permutations (map fromPartition rP)
    let winnigFieldsDistribution = histogram $ map (winnigFields testeeComposition) rC
        winnigFieldsMeanRelative = mean winnigFieldsDistribution / fromIntegral m
    putStrLn "Winning Fields:"
    putStrLn $ "\tDistribution: " ++ show winnigFieldsDistribution
    putStrLn $ "\tMean / m = " ++ show winnigFieldsMeanRelative
    let scoreDistribution = histogram $ map (score testeeComposition) rC
        scoreMeanRelative = mean scoreDistribution / fromIntegral m
    putStrLn "Winning Fields - Lost Fields:"
    putStrLn $ "\tDistribution: " ++ show scoreDistribution
    putStrLn $ "\tMean / m = " ++ show scoreMeanRelative