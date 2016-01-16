{-# LANGUAGE BangPatterns #-}

module Sample (sample, randomElement) where

import Control.Monad.Primitive
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import System.Random.MWC

sample :: PrimMonad m => [a] -> Int -> Gen (PrimState m) -> m [a]
sample ys size = go 0 (l - 1) (Seq.fromList ys) where
    l = length ys
    go !n !i xs g | n >= size = return $! (toList . Seq.drop (l - size)) xs
                  | otherwise = do
                      j <- uniformR (0, i) g
                      let toI  = xs `Seq.index` j
                          toJ  = xs `Seq.index` i
                          next = (Seq.update i toI . Seq.update j toJ) xs
                      go (n + 1) (i - 1) next g
{-# INLINE sample #-}

randomElement :: PrimMonad m => [a] -> Gen (PrimState m) -> m a
randomElement [] _ = error "empty list"
randomElement ys g = do
    i <- uniformR (0, length ys - 1) g
    return $ ys !! i
{-# INLINE randomElement #-}