{-# LANGUAGE ExistentialQuantification, FlexibleContexts,
      FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

import Lotto
-- import Sample
import System.Random
import System.Random.Shuffle
-- import Utils
-- import qualified Graphics.Gnuplot.Terminal.X11 as X11
-- import Graphics.Gnuplot.Simple
import Control.Monad.State hiding (state)
import Control.Monad.Loops (iterateWhile)
import qualified Data.Vector as V
import Math.Combinat.Partitions

type GameM g a = State (GameContext g) a

data GameContext g = GameContext {
    game :: g,
    year :: Int,
    state :: GameState g
}

class Game g where
    type GameState g
    isOver :: GameContext g -> Bool
    play :: GameContext g -> GameState g

update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get

step :: (Game g) => GameM g (GameContext g)
step = update (\gc -> gc { year = year gc + 1, state = play gc })

iterateGame :: (Game g) => GameM g ()
iterateGame = do
    _ <- iterateWhile (not . isOver) step
    return ()

newGame :: forall g. g -> GameState g -> GameContext g
newGame g s = GameContext { game = g, year = 0, state = s }

execGame :: (Game g) => GameContext g -> GameContext g
execGame = execState iterateGame

---

data Lotto = Lotto { maxYear :: Int }

data LottoPlayer = LottoPlayer {
    name :: String,
    strategies :: V.Vector Partition
}

instance Show LottoPlayer where
    show p = show (name p) ++ " (" ++ (show . V.length . strategies) p ++ "): " ++ show (strategies p)

data LottoState = LottoState {
    playerA :: LottoPlayer,
    playerB :: LottoPlayer,
    randomGen :: StdGen
}

instance Show LottoState where
    show s = show (playerA s) ++ "\n\n" ++ show (playerB s)

instance Game Lotto where
    type GameState Lotto = LottoState
    isOver gc = year gc >= maxYear (game gc) ||
                (V.length . strategies . playerA . state) gc == 0 ||
                (V.length . strategies . playerB . state) gc == 0
    play gc = newState
        where
            s = state gc
            (pA, pB) = (playerA s, playerB s)
            (sA, sB) = (strategies pA, strategies pB)
            (indA, genA) = randomR (0, V.length sA - 1) (randomGen s)
            (indB, genB) = randomR (0, V.length sB - 1) genA
            (a, b) = (sA V.! indA, sB V.! indB)
            result = force a b
            newPA
                | result > 0 = pA { strategies = V.snoc sA a }
                | result == 0 = pA
                | otherwise = pA { strategies = remove indA sA }
            newPB
                | -result > 0 = pB { strategies = V.snoc sB b }
                | result == 0 = pB
                | otherwise = pB { strategies = remove indB sB }
            newState = s { playerA = newPA, playerB = newPB, randomGen = genB }

remove :: forall a. Int -> V.Vector a -> V.Vector a
remove ind vec = V.take ind vec V.++ V.drop (ind + 1) vec

sample :: forall gen a. RandomGen gen => gen -> Int -> [a] -> [a]
sample gen sz xs = take sz $ shuffle' xs (length xs) gen


main :: IO ()
main = print $ state $ execGame lottoGame
    where
        lottoGame = newGame Lotto { maxYear = 1000 } LottoState { playerA = pA, playerB = pB, randomGen = mkStdGen seed }
        pA = LottoPlayer { name = "S", strategies = V.fromList (sample (mkStdGen seed) 100 (filter isStrong allP)) }
        pB = LottoPlayer { name = "W", strategies = V.fromList (sample (mkStdGen seed) 100 (filter isWeak allP)) }
        allP = allLottoPartitions n m
        n = 36
        m = 6
        seed = 1
        isStrong p = eigenResource p > maxEigenResource m - 1
        isWeak p = eigenResource p > maxEigenResource m - 5
