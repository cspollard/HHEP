module Main where


import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Graphics.Histo

import Data.Histogram

hTest :: Histo1D
hTest = histogram (constBin1D 10 (0, 10)) mempty `fill` (1.0, Z :. 1.5) `fill` (2.5, Z :. 2.5) `fill` (0.5, Z :. 2.5)

main :: IO ()
-- main = mainWith (point ((1.0, (0.25, 0.5)), (0.0, (0.5, 0.25))) # lw 10 # showOrigin:: Diagram B)
main = mainWith (drawHist hTest # lw 10 # showOrigin :: Diagram B)
