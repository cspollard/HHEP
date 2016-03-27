module Main where


import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.PGF.CmdLine
import Graphics.Histo

import Data.Histogram

hTest :: Histo1D
hTest = histogram (constBin1D 10 (0, 10)) mempty `fill` (1.0, Z :. 1.5) `fill` (2.5, Z :. 2.5) `fill` (0.5, Z :. 2.5)

hTest' :: Histo1D
hTest' = histogram (constBin1D 10 (0, 10)) mempty `fill` (1.0, Z :. 1.5) `fill` (0.5, Z :. 3.5) `fill` (0.5, Z :. 0.5)

main :: IO ()
main = (mainWith :: Diagram B -> IO ()) $
        let h  = drawGraph $ histToGraph hTest
            h' = drawGraph $ histToGraph hTest'
        in mconcat [ h  # lc blue # lw 05 # dashing [10, 10] 0
                   , h' # lc red  # lw 05
                   , yAxis h
                   , xAxis h
                   , text "$e = mc^2$" # fontSizeG 1 # fc green # translateX 5 # translateY 2
                   ] # centerXY # pad 1.1
