{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample.Histogram as H

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Csv as Csv
import Statistics.Function (minMax)

import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)

import System.IO


{-------------------------------------------------------------------------------
  Histograms
-------------------------------------------------------------------------------}

histogram :: (Ord a) => [a] -> [(a, Int)]
histogram = Map.assocs . histogram'

histogram' :: (Ord a) => [a] -> Map.Map a Int
histogram' = foldl addElem Map.empty
  where addElem m e = Map.insertWith (+) e 1 m

binnedHistogram :: Int -> [Double] -> [(Double, Int)]
binnedHistogram bins xs = zip [lo,lo + (hi - lo)/(fromIntegral bins - 1)..hi] (V.toList h)
  where (_, h) = H.histogram bins es :: (V.Vector Double, V.Vector Int)
        es = V.fromList xs
        (lo, hi) = minMax es

{-------------------------------------------------------------------------------
  CSV
-------------------------------------------------------------------------------}

saveCSV :: Csv.ToRecord a => FilePath -> [String] -> [a] -> IO ()
saveCSV name h xs = Prelude.writeFile name $ Char8.unpack $ toCSV (map Char8.pack h) xs
  where toCSV hs xs' = Char8.append header (encode' xs')
          where header = Char8.append (Char8.intercalate "," hs) "\n"
                encode' = Csv.encodeWith (Csv.defaultEncodeOptions { Csv.encUseCrLf = False })


{-------------------------------------------------------------------------------
  Dump to Directory
-------------------------------------------------------------------------------}

setExperimentDir :: String -> IO ()
setExperimentDir experimentName = do
    currentTime <- getZonedTime
    let experimentDir = "output/" ++ experimentName ++ "-" ++ formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime
    createDirectoryIfMissing True experimentDir
    setCurrentDirectory experimentDir

putStrLnBoth :: Handle -> String -> IO ()
putStrLnBoth h str = do
    putStrLn str
    hPutStrLn h str
