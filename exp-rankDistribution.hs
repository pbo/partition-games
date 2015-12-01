import Lotto
-- import Utils
import Math.Combinat.Partitions
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.X11 as X11
import Data.List
import Text.Tabular
import Text.Tabular.AsciiArt
import qualified Data.MultiMap as MM
import qualified Data.Map as M

rank :: Int -> Partition -> Int
rank _ (Partition []) = error "Empty partition"
rank n (Partition (x:_)) = n - x

maxRank :: Int -> Int -> Int
maxRank n m  = n - ceiling ((fromIntegral n :: Double) / fromIntegral m)

distance' :: Partition -> Partition -> Int
distance' (Partition xs) (Partition ys) = maximum (map abs (zipWith (-) xs ys))

family :: [Partition] -> Int -> Partition -> [Partition]
family ps d p = filter (\p' -> distance' p p' <= d) ps


mean xs = (fromIntegral (sum xs) :: Double) / fromIntegral (length xs)

main :: IO ()
main =  do
    let (n, m) = (49, 7)
        ps = allLottoPartitions n m
        mp = M.fromList $ map (\((er, r), s) -> ((er, r), (mean s, mean s / resourseSize er))) $ (MM.assocs . MM.fromList) $ map (\p -> ((eigenResource p, rank n p), familySize p)) ps
        -- rs = sort (map (\((er, r), s) -> (er, r, mean s, mean s / resourseSize er))
                -- ((MM.assocs . MM.fromList) (map (\p -> ((eigenResource p, rank n p), familySize p)) ps)))
        familySize p = length (family ps 1 p)
        resourseSize r = fromIntegral (length (filter (\p -> eigenResource p == r) ps))
        -- table = Table (Group NoLine (map (Header . show) [1..length rs]))
            -- (Group SingleLine [Header "Resource", Header "Rank", Header "Abs. Family Size", Header "Rel. Family Size"])
            -- (map (\(er, r, s, rels) -> [show er, show r, show s, show rels]) rs)
        -- partitionsWithRank r = filter (\p -> rank n p == r) ps
        -- rs = [(r, (length . partitionsWithRank) r) | r <- [0..maxrank n m]] -- Number of paritions with specified rank
        -- rs = [(r, histogram (map familySize (partitionsWithRank r))) | r <- [0..maxrank n m]] -- Histogram of capacity of families which head partition has specified rank
        -- rs = map (\(r, s) -> (r, mean s / resourseSize r)) ((MM.assocs . MM.fromList) (map (eigenResource &&& familySize) ps))
        -- plotTitle = "n = " ++ show n ++ ", m = " ++ show m
        -- plotHistogram t title = plotPathStyle [t, Title plotTitle] PlotStyle {plotType=Boxes, lineSpec=CustomStyle [LineTitle title]}
    -- plotHistogram (PNG "familySizeDistribution.png") "Family Size Distribution" rs
    -- putStrLn $ render id id id table
    -- terminal X11.T
    let mesh3d = [[(fromIntegral er :: Double, fromIntegral r :: Double, snd (M.findWithDefault (0, 0) (er, r) mp)) | r <- [0..maxRank n m]] | er <- [0..maxEigenResource m]]
    plotMesh3d [Title "EigenResource, Rank, Family Size"] [Plot3dType Surface, CornersToColor Mean] mesh3d
    plotMesh3d [PNG "familySizeDistribution.png", Title "EigenResource, Rank, Family Size"] [Plot3dType ColorMap, CornersToColor Mean] mesh3d
    return ()