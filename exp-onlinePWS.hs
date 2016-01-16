import Lotto
import Sample
import Utils
import Math.Combinat.Partitions
import Control.Monad.Primitive (PrimState, PrimMonad)
import Control.Monad (foldM, forM)
import Control.Applicative()
import System.Random.MWC (Gen, initialize)
import qualified Math.Combinatorics.Multiset as MS
import qualified Data.Vector as V
import qualified Data.Sequence as S
import Graphics.Gnuplot.Simple
import System.IO
import Statistics.Sample


allBlottoPartitionsWithoutZeros :: Int -> Int -> [[Int]]
allBlottoPartitionsWithoutZeros n m = map fromPartition (partitionsWithKParts m n) >>= MS.permutations . MS.fromList


filterSellers :: [Int] -> Int -> [[Int]] -> [[Int]]
filterSellers buyer delta = filter (all (\diff -> abs diff < delta) . zipWith (-) buyer)


sampleSellers :: PrimMonad m => [Int] -> [[Int]] -> Int -> Gen (PrimState m) -> m [[Int]]
sampleSellers buyer sellers size gen = do
    sellersMin <- forM (zip [0..] buyer) (\(i, buyerBin) ->
        randomElement (filter (\s -> s !! i <= buyerBin) sellers) gen
        )
    restSellers <- sample sellers (min (size - length sellersMin) (length sellers)) gen
    return $ sellersMin ++ restSellers


-- reservationPartition :: [Int] -> [Int]
-- reservationPartition buyer = map p' buyer
--     where
--         n = sum buyer
--         p' p = floor (sqrt ((fromIntegral n - fromIntegral p) * fromIntegral p) :: Double)

data Stats = Stats { paid :: S.Seq Int, time :: Int }

emptyStats :: Int -> Stats
emptyStats m = Stats { paid = S.replicate m 0, time = 0 }

nextStats :: Stats -> [Int] -> [Int] -> Int -> Stats
nextStats currentStats buyer seller binIndex
        | all (>0) currentPaid = currentStats
        | S.index currentPaid binIndex == 0 && sellerBin <= buyerBin =
            Stats { paid = S.update binIndex sellerBin currentPaid, time = time currentStats + 1 }
        | otherwise =
            Stats { paid = currentPaid, time = time currentStats + 1 }
    where
        sellerBin = seller !! binIndex
        buyerBin = buyer !! binIndex
        currentPaid = paid currentStats

main :: IO ()
main = do
    let (n, m) = (49, 7)
        buyer = uniform' n m
        buyerR = map (+ 2) buyer
        allPs = allBlottoPartitionsWithoutZeros n m
        sellersBinDelta = 4
        sellersCount = 200
        suitableSellers = filterSellers buyer sellersBinDelta allPs
        experimentsCount = 1000

    allStats <- forM [0..experimentsCount] (\seed -> do
        gen <- initialize (V.fromList [seed])
        sellers <- sampleSellers buyerR suitableSellers sellersCount gen

        let indices = [(sellerIndex, binIndex) | sellerIndex <- [0 .. length sellers - 1], binIndex <- [0 .. m - 1]]
        shuffledIndices <- sample indices (length indices) gen
        foldM (\st (si, bi) -> return $ nextStats st buyerR (sellers !! si) bi) (emptyStats m) shuffledIndices
        )

    let paidSums = map (sum . paid) allStats
        times = map time allStats

    -- Set Directory Output
    setExperimentDir "onlinePWS"

    -- Plot Stats
    let paidSumsH = histogram paidSums
        timesH = histogram times
        plotTitle = "n = " ++ show n ++ ", m = " ++ show m
        plotHistogram t title = plotPathStyle [t, Title plotTitle] PlotStyle {plotType=Boxes, lineSpec=CustomStyle [LineTitle title]}
    plotHistogram (PNG "paidSums.png") "Cost Distribution" paidSumsH
    plotHistogram (PNG "timesPassed.png") "Time Passed Distribution" timesH
    -- plotHistogram (terminal (X11.persist X11.cons)) "Distance Max" hMax

    -- Dump Log
    _ <- withFile "log.txt" WriteMode (\h -> do
        putStrLnBoth h "INPUT"
        putStrLnBoth h $ "n = " ++ show n
        putStrLnBoth h $ "m = " ++ show m
        putStrLnBoth h $ "Buyer = " ++ show buyer
        putStrLnBoth h $ "Buyer Threshold = " ++ show buyerR
        putStrLnBoth h $ "Sellers Delta = " ++ show sellersBinDelta
        putStrLnBoth h $ "Selected Sellers Count = " ++ show sellersCount
        putStrLnBoth h $ "Runs Count = " ++ show experimentsCount
        putStrLnBoth h ""
        putStrLnBoth h "OUTPUT"
        putStrLnBoth h $ "All Suitable Sellers Count = " ++ show (length suitableSellers)
        putStrLnBoth h $ "Actual Sellers Count in Experiment = " ++ show (min (length suitableSellers) sellersCount)
        putStrLnBoth h $ "Paid Sums Distribution: " ++ show paidSumsH

        putStrLnBoth h $ "\tMean: " ++ show (mean (V.fromList (map fromIntegral paidSums)))
        putStrLnBoth h $ "\tVariance: " ++ show (variance (V.fromList (map fromIntegral paidSums)))
        putStrLnBoth h $ "\tSkewness: " ++ show (skewness (V.fromList (map fromIntegral paidSums)))

        putStrLnBoth h $ "Time Passed Distribution: " ++ show timesH
        putStrLnBoth h $ "\tMean: " ++ show (mean (V.fromList (map fromIntegral times)))
        putStrLnBoth h $ "\tVariance: " ++ show (variance (V.fromList (map fromIntegral times)))
        putStrLnBoth h $ "\tSkewness: " ++ show (skewness (V.fromList (map fromIntegral times)))
        return ())

    -- Save CSVs
    saveCSV "paidSums.csv" ["paid", "count"] paidSumsH
    saveCSV "timesPassed.csv" ["timePassed", "count"] paidSumsH

    return ()