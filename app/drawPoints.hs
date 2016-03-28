module Main where


import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.PGF.CmdLine
import Data.Colour.Palette.ColorSet

import Graphics.Histo
import Data.Histogram

import Debug.Trace

hTest :: Histo1D
hTest = histogram (constBin1D 10 (0, 10)) mempty `fill` (-1.0, Z :. 1.5) `fill` (2.5, Z :. 2.5) `fill` (0.5, Z :. 2.5)

hTest' :: Histo1D
hTest' = histogram (constBin1D 10 (0, 5)) mempty `fill` (1.0, Z :. 1.5) `fill` (0.5, Z :. 3.5) `fill` (0.5, Z :. 0.5)

main :: IO ()
main = (mainWith :: Diagram B -> IO ()) $
        let h  = drawGraph $ histToGraph hTest
            h' = drawGraph $ histToGraph hTest'
        in mconcat [ h  # lc (d3Colors1 0)
                   , h' # lc (d3Colors1 1)
                   -- , text "$e = mc^2$" # fontSizeG 1 # fc (d3Colors1 2) # translateX 5 # translateY 2
                   ] # scaleToX 1 # scaleToY 5 # addAxes # centerXY # pad 1.1 # lwN 0.005
