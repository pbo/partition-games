import Lotto
import Utils
import Sample
import GHC.Prim (RealWorld)
import Math.Combinat.Partitions
import System.Locale (defaultTimeLocale)
import System.IO
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.Console.Haskeline
import System.Random.MWC (Gen, initialize)
import Control.Monad
import Control.Applicative hiding (empty)
import Control.Arrow (second, (***))
import Control.Lens
import Data.Time (getZonedTime, formatTime)
import Data.Time.Format()
import Data.List (sortBy)
import Data.Char (toUpper)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Vector.Unboxed (fromList)
import qualified Data.Vector as V
import Data.Map (Map, empty, insert, assocs)
import Graphics.Rendering.Chart
import Data.Default.Class (def)
import Data.Colour (opaque)
import Data.Colour.Names (gray)
import Graphics.Rendering.Chart.Backend.Cairo


intro :: String
intro = "+------------------------------------------+\n" ++
        "| EVOLUTIONARY DYNAMICS OF PARTITION GAMES |\n" ++
        "| Simulation Modeling Tool                 |\n" ++
        "| v. 0.1                                   |\n" ++
        "| Created by Pavel Bocharov in 2015        |\n" ++
        "+------------------------------------------+"

putHeader :: String -> IO ()
putHeader str = putStrLn $ "\n" ++ map toUpper str ++ "\n"

promptLine :: String -> String -> String -> IO String
promptLine prompt defaultValue comment = do
    result <- runInputT defaultSettings $ do
        let promptStr = map toUpper prompt ++ (if null comment then "" else "\n" ++ comment) ++ "\n> "
        maybeInput <- getInputLineWithInitial promptStr (defaultValue, "")
        case maybeInput of
            Nothing -> return defaultValue
            Just input -> return input
    putStr "\n"
    return result

putStrLnBoth :: Handle -> String -> IO ()
putStrLnBoth h str = do
    putStrLn str
    hPutStrLn h str

putStrBoth :: Handle -> String -> IO ()
putStrBoth h str = do
    putStr str
    hPutStr h str

main :: IO ()
main = do
    putStrLn intro
    currentTime <- getZonedTime
    putHeader $ "Experiment " ++ show currentTime
    putHeader "Input Data"
    n <- read <$> promptLine "N" "100" "Number of resources"
    m <- read <$> promptLine "M" "10" "Number of fields"
    pSize <- promptLine "Size of initial population" "0.1%" "Enter absolute value (e. g. 1000) or relative (e. g. 42%)"
    pSeed <- read <$> promptLine "Initial population random seed number" "1" "It will uniquely identifies an initial population"
    eSeed <- read <$> promptLine "Evolution random seed number" "1" "It will uniquely identifies an evolutional process"
    w <- read <$> promptLine "w" (show (evenly m n)) "Environmental impact vector"
    let maxThreshold = 2 - 2 / fromIntegral m :: Double
    threshold <- read <$> promptLine "h" (show (maxThreshold / 2)) ("Modification gain threshold, from 0 to (2 - 2/m) = " ++ show maxThreshold)
    sigma <- read <$> promptLine "sigma" "20" "Modification gain sigma"
    thisEndOfTime <- read <$> promptLine "T" "7000" "Max number of iterations (\"years\")"
    thisStatsResolution <- read <$> promptLine "Frequency of statistical slices" (show (thisEndOfTime `div` 20)) "Tweak it to get more or less detalized statistics"

    let experimentDir = "output/evolution-" ++ formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime
    createDirectoryIfMissing True experimentDir
    setCurrentDirectory experimentDir

    putHeader "Evolution..."
    let classAll = allLottoPartitions n m
        k = length classAll
        pSizeAbs = if last pSize == '%' then floor ((read (init pSize) :: Float) * fromIntegral k / 100.0) else read pSize
    putStrLn $ "Number of all (" ++ show n ++ ", " ++ show m ++ ")-paritions: " ++ show k
    putStrLn $ "Number of partitions in the initial population: " ++ show pSizeAbs
    initialPopulationGen <- initialize (fromList [pSeed])
    firstPopulation <- sample classAll pSizeAbs initialPopulationGen
    evolutionGen <- initialize (fromList [eSeed])
    let settings = EvolutionSettings {
            statsResolution = thisStatsResolution,
            endOfTime = thisEndOfTime,
            scrambleSettings = ScrambleSettings {
                gameF = force,
                gainF = gain threshold sigma w }}
    (lastPopulation, allStats) <- evolution evolutionGen settings (V.fromList firstPopulation)
    putStrLn "Simulation finished."

    dumpStats allStats
    _ <- withFile "log.txt" WriteMode (\h -> do
        hPutStrLn h $ "Experiment " ++ show currentTime
        hPutStrLn h ""
        hPutStrLn h "INPUT DATA"
        hPutStrLn h $ "N = " ++ show n
        hPutStrLn h $ "M = " ++ show m
        hPutStrLn h $ "Size of initial population = " ++ pSize
        hPutStrLn h $ "Initial population random seed number = " ++ show pSeed
        hPutStrLn h $ "Evolution random seed number = " ++ show eSeed
        hPutStrLn h $ "W = " ++ show w
        hPutStrLn h $ "H = " ++ show threshold
        hPutStrLn h $ "SIGMA = " ++ show sigma
        hPutStrLn h $ "T = " ++ show thisEndOfTime
        hPutStrLn h $ "Frequency of statistical slices = " ++ show thisStatsResolution
        hPutStrLn h ""
        let cLast = V.length lastPopulation
        putStrLnBoth h $ "The last population contains " ++ show cLast ++ " partitions. "
        if cLast > 200
            then putStrBoth h "Here are the first 200 of them:\n"
            else putStrBoth h "Here they are:\n"
        _ <- forM (take 200 (V.toList lastPopulation)) (\(Partition p) -> putStrLnBoth h (show p))
        return ())
    return ()

data ScrambleSettings = ScrambleSettings {
    gameF :: Partition -> Partition -> Int,
    gainF :: Partition -> Int }

scramble :: ScrambleSettings -> V.Vector Partition -> (Int, Int) -> V.Vector Partition
scramble settings population (aInd, bInd) =
    case signum (gameF settings a b) of
        -1 -> remove aInd (population V.// [(bInd, strengthen (proportionaly b' (gainF settings b)) b)])
        1  -> remove bInd (population V.// [(aInd, strengthen (proportionaly a' (gainF settings a)) a)])
        _  -> population
    where remove ind vec = V.take ind vec V.++ V.drop (ind + 1) vec
          (a, b) = (population V.! aInd, population V.! bInd)
          (Partition a', Partition b') = (a, b)

gain :: Double -> Int -> [Int] -> Partition -> Int
gain threshold sigma w (Partition a) = if d < threshold then round (fromIntegral sigma / (d + 1) :: Double) else 0
    where d = distance (sortBy (flip compare) w) a
          distance xs ys = sum $ zipWith (\x y -> abs (x - y)) (normalize xs) (normalize ys)

strengthen :: [Int] -> Partition -> Partition
strengthen rv (Partition a) = toPartitionUnsafe $ zipWith (+) a rv

proportionaly :: [Int] -> Int -> [Int]
proportionaly ms n = roundToSum n $ map (* fromIntegral n) (normalize ms) -- sort??

normalize :: [Int] -> [Double]
normalize xs = if s == 0 then map fromIntegral xs else map ((/ fromIntegral s) . fromIntegral) xs
    where s = sum xs

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

evolution :: Gen RealWorld -> EvolutionSettings -> V.Vector Partition -> IO (V.Vector Partition, Map Int Stats)
evolution = evolution' empty 0
    where
        report t p = do
            putStrLn $ "Year " ++ show t ++ ", population: " ++ show (V.length p)
            hFlush stdout
        evolution' allStats time gen settings population =
            if (time >= endOfTime settings) || (V.length population < 2)
                then do
                    report time population
                    return (population, insert time (stats population) allStats)
                else do
                    when (time `mod` statsResolution settings == 0) $ report time population
                    let newAllStats = if time `mod` statsResolution settings == 0 then insert time (stats population) allStats else allStats
                    [aInd, bInd] <- sample [0..(V.length population - 1)] 2 gen
                    let newPopulation = scramble (scrambleSettings settings) population (aInd, bInd)
                    evolution' newAllStats (time + 1) gen settings newPopulation

data Stats = Stats {
    eigenResourceDistribution :: Map Int Int,
    sumResourceDistribution :: Map Int Int,
    populationCount :: Int }

stats :: V.Vector Partition -> Stats
stats population = Stats {
    eigenResourceDistribution = histogram' $ V.toList (V.map eigenResource population),
    sumResourceDistribution = histogram' $ V.toList (V.map (\(Partition p) -> sum p) population),
    populationCount = V.length population }

dumpStats :: Map Int Stats -> IO ()
dumpStats allStats = do
    createDirectoryIfMissing False "eigenResourceDistribution"
    createDirectoryIfMissing False "sumResourceDistribution"
    let allStatsAccos = assocs allStats
    _ <- forM allStatsAccos (\(t, s) -> do
        saveCSV ("eigenResourceDistribution/" ++ show t ++ ".csv") [pack "eigenResource", pack "count"] $ assocs $ eigenResourceDistribution s
        saveCSV ("sumResourceDistribution/" ++ show t ++ ".csv") [pack "sumResource", pack "count"] $ assocs $ sumResourceDistribution s)
    saveCSV "populationCount.csv" [pack "population"] $ map (second populationCount) allStatsAccos
    let convertStats f = map (show *** convS f)
        convS f s = map (fromIntegral *** ((:[]) . fromIntegral)) $ assocs (f s)
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