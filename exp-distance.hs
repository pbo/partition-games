import Lotto
import Utils
-- import qualified Graphics.Gnuplot.Terminal.X11 as X11
import Graphics.Gnuplot.Simple

main :: IO ()
main = do
    let (n, m) = (36, 6)
        ps = allLottoPartitions n m
        u = uniform n m
        h = histogram $ map (distance u) ps
        hMax = histogram $ map (distanceMax u) ps
        plotTitle = "n = " ++ show n ++ ", m = " ++ show m
        plotHistogram t title = plotPathStyle [t, Title plotTitle] PlotStyle {plotType=Boxes, lineSpec=CustomStyle [LineTitle title]}
    plotHistogram (PNG "distance.png") "Distance" h
    plotHistogram (PNG "distanceMax.png") "Distance Max" hMax
    -- plotHistogram (terminal (X11.persist X11.cons)) "Distance Max" hMax
    return ()