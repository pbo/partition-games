import Lotto
import Utils
import Math.Combinat.Partitions
import Sample
import System.IO
import System.Directory (createDirectoryIfMissing)
import System.Console.Haskeline
import System.Random.MWC
import Control.Monad
import Control.Applicative
import Data.Time
import GHC.Prim
import Data.Char
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.ByteString.Lazy.Char8 (pack)
import Control.Arrow (second, (***))
import Graphics.Rendering.Chart
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import Data.Colour
import Data.Colour.Names

intro :: String
intro = "+------------------------------------------+\n" ++
        "| EVOLUTIONARY DYNAMICS OF PARTITION GAMES |\n" ++
        "| Simulation Modeling Tool                 |\n" ++
        "| v. 0.1                                   |\n" ++
        "| Created by Pavel Bocharov in 2015        |\n" ++
        "+------------------------------------------+"

putHeader :: String -> IO ()
putHeader str = putStrLn $ "\n" ++ map toUpper str

promptLine :: String -> String -> String -> IO String
promptLine prompt defaultValue comment = runInputT defaultSettings $ do
    let promptStr = map toUpper prompt ++ (if null comment then "" else ", " ++ comment) ++ ": "
    maybeInput <- getInputLineWithInitial promptStr (defaultValue, "")
    case maybeInput of
        Nothing -> return defaultValue
        Just input -> return input

main :: IO ()
main = do
    putStrLn intro
    currentTime <- getZonedTime
    putHeader $ "Experiment " ++ show currentTime
    putHeader "Input Data"
    n <- read <$> promptLine "N" "100" "number of resources"
    m <- read <$> promptLine "M" "10" "number of fields"
    pSize <- promptLine "Size of initial population" "0.1%" "enter absolute value (e. g. 1000) or relative (e. g. 42%)"
    pSeed <- read <$> promptLine "Initial population random seed number" "1" "it will uniquely identifies an initial population"
    eSeed <- read <$> promptLine "Evolution random seed number" "1" "it will uniquely identifies an evolutional process"
    w <- read <$> promptLine "w" (show (evenly m n)) "environmental impact vector"
    threshold <- read <$> promptLine "h" "40" "modification gain threshold"
    sigma <- read <$> promptLine "sigma" "20" "modification gain sigma"
    thisEndOfTime <- read <$> promptLine "T" "7000" "max number of iterations (\"years\")"
    thisStatsResolution <- read <$> promptLine "Frequency of statistical slices" (show (thisEndOfTime `div` 20)) "tweak it to get more or less detalized statistics"
    putHeader "Evolution..."
    let classAll = allLottoPartitions n m
        k = length classAll
        pSizeAbs = if last pSize == '%' then floor ((read (init pSize) :: Float) * fromIntegral k / 100.0) else read pSize
    putStrLn $ "Number of all (" ++ show n ++ ", " ++ show m ++ ")-paritions: " ++ show k
    putStrLn $ "Number of partitions in the initial population: " ++ show pSizeAbs
    initialPopulationGen <- initialize (VU.fromList [pSeed])
    firstPopulation <- sample classAll pSizeAbs initialPopulationGen
    evolutionGen <- initialize (VU.fromList [eSeed])
    let settings = EvolutionSettings {
            statsResolution = thisStatsResolution,
            endOfTime = thisEndOfTime,
            scrambleSettings = ScrambleSettings {
                gameF = force,
                gainF = gain threshold sigma w }}
    (lastPopulation, allStats) <- evolution evolutionGen settings (V.fromList firstPopulation)
    putStrLn "Simulation finished."
    dumpStats allStats
    let cLast = V.length lastPopulation
    putStr $ "The last population contains " ++ show cLast ++ " partitions. "
    if cLast > 200
        then putStr "Here are the first 200 of them:\n"
        else putStrLn "Here they are:\n"
    _ <- forM (take 200 (V.toList lastPopulation)) (\(Partition p) -> print p)
    return ()

data ScrambleSettings = ScrambleSettings {
    gameF :: Partition -> Partition -> Int,
    gainF :: Partition -> Int }

scramble :: ScrambleSettings -> V.Vector Partition -> (Int, Int) -> V.Vector Partition
scramble settings population (aInd, bInd) =
    case signum (gameF settings a b) of
        -1 -> remove aInd (population V.// [(bInd, strengthen (proportionaly b' (gainF settings b)) b)]) -- (evenly (length b') (gainF settings b))
        1  -> remove bInd (population V.// [(aInd, strengthen (proportionaly a' (gainF settings a)) a)]) -- (evenly (length a') (gainF settings a))
        _  -> population
    where remove ind vec = V.take ind vec V.++ V.drop (ind + 1) vec
          (a, b) = (population V.! aInd, population V.! bInd)
          (Partition a', Partition b') = (a, b)

gain :: Int -> Int -> [Int] -> Partition -> Int
gain threshold sigma w (Partition a) = if d < threshold then round (fromIntegral sigma / fromIntegral d :: Double) else 0
    where d = distance w a -- sigma * d - Чем больше разбиение отличается от w, тем сильнее его поощряет среда. Это верно?
          distance xs ys = sum $ zipWith (\x y -> abs(x - y)) xs ys

strengthen :: [Int] -> Partition -> Partition
strengthen rv (Partition a) = toPartitionUnsafe $ zipWith (+) a rv

proportionaly :: [Int] -> Int -> [Int]
proportionaly ms n = roundToSum n (map (\m -> fromIntegral n * fromIntegral m / fromIntegral (sum ms)) ms)

roundToSum :: Int -> [Double] -> [Int]
roundToSum _ [] = []
roundToSum s [_] = [s]
roundToSum s (x:xs) = x' : roundToSum (s - x') xs'
    where x' = round x :: Int
          rs = repeat ((x - fromIntegral x') / fromIntegral (length xs))
          xs' = zipWith (+) xs rs

evenly :: Int -> Int -> [Int]
evenly m n = zipWith (+) (replicate m q) (replicate r 1 ++ repeat 0)
    where (q, r) = n `quotRem` m

data EvolutionSettings = EvolutionSettings {
    statsResolution :: Int,
    endOfTime :: Int,
    scrambleSettings :: ScrambleSettings}

evolution :: Gen RealWorld -> EvolutionSettings -> V.Vector Partition -> IO (V.Vector Partition, M.Map Int Stats)
evolution = evolution' M.empty 0
    where
        report t p = do
            putStrLn $ "Year " ++ show t ++ ", population: " ++ show (V.length p)
            hFlush stdout
        evolution' allStats time gen settings population =
            if (time >= endOfTime settings) || (V.length population < 2)
                then do
                    report time population
                    return (population, M.insert time (stats population) allStats)
                else do
                    when (time `mod` statsResolution settings == 0) $ report time population
                    let newAllStats = if time `mod` statsResolution settings == 0 then M.insert time (stats population) allStats else allStats
                    [aInd, bInd] <- sample [0..(V.length population - 1)] 2 gen
                    let newPopulation = scramble (scrambleSettings settings) population (aInd, bInd)
                    evolution' newAllStats (time + 1) gen settings newPopulation

data Stats = Stats {
    eigenResourceDistribution :: M.Map Int Int,
    sumResourceDistribution :: M.Map Int Int,
    populationCount :: Int }

stats :: V.Vector Partition -> Stats
stats population = Stats {
    eigenResourceDistribution = histogram' $ V.toList (V.map eigenResource population),
    sumResourceDistribution = histogram' $ V.toList (V.map (\(Partition p) -> sum p) population),
    populationCount = V.length population }

dumpStats :: M.Map Int Stats -> IO ()
dumpStats allStats = do
    createDirectoryIfMissing False "eigenResourceDistribution"
    createDirectoryIfMissing False "sumResourceDistribution"
    let allStatsAccos = M.assocs allStats
    _ <- forM allStatsAccos (\(t, s) -> do
        saveCSV ("eigenResourceDistribution/" ++ show t ++ ".csv") [pack "eigenResource", pack "count"] $ M.assocs $ eigenResourceDistribution s
        saveCSV ("sumResourceDistribution/" ++ show t ++ ".csv") [pack "sumResource", pack "count"] $ M.assocs $ sumResourceDistribution s)
    saveCSV "populationCount.csv" [pack "population"] $ map (second populationCount) allStatsAccos
    let convertStats f = map (show *** convS f)
        convS f s = map (fromIntegral *** ((:[]) . fromIntegral)) $ M.assocs (f s)
    let chartFileOptions = fo_size .~ (1024, 200 * length allStatsAccos) $ def
    _ <- ($) renderableToFile chartFileOptions "eigenResourceDistribution.png" $ historyChart (convertStats eigenResourceDistribution allStatsAccos)
    _ <- ($) renderableToFile chartFileOptions "sumResourceDistribution.png" $ historyChart (convertStats sumResourceDistribution allStatsAccos)
    putStrLn "Results saved."
    return ()

historyChart :: [(String, [(Double, [Double])])] -> Renderable ()
historyChart values = renderStackedLayouts $ def & slayouts_layouts .~ map (makeLayout . second histPlot) values
  where makeLayout (title, plot) =
          StackedLayout $ layout_title .~ title
                        $ layout_plots .~ [plot]
                        $ def
        histPlot xs =
            plotBars
                $ plot_bars_values .~ xs
                $ plot_bars_style .~ BarsStacked
                $ plot_bars_spacing .~ BarsFixGap 2 0
                $ plot_bars_item_styles .~ [(solidFillStyle (opaque gray), Nothing)]
                $ def