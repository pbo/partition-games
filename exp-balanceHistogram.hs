{-# LANGUAGE OverloadedStrings #-}

import Lotto
import Utils

main :: IO ()
main = saveCSV filename ["balance", "count"] $ histogram $ map balance ps
  where
    filename = "output/balance-n=" ++ show n ++ "-m=" ++ show m ++ ".csv"
    ps = allLottoPartitions n m
    n = 100
    m = 10