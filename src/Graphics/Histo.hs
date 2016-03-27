{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts #-}

module Graphics.Histo where

import Data.TypeList
import Diagrams.Prelude
import Data.Histogram
import Control.Arrow ((&&&))


dup :: a -> (a, a)
dup x = (x, x)

drawHist :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) =>
            Histo1D -> Diagram b
drawHist h = mconcat . map (point . toPoint) $ toTuples h
    where
        toPoint ((Z :. xlo, Z :. xhi), d :. _) = let x0 = (xlo+xhi)/2.0 in
                                    ((x0, (xhi-x0, x0-xlo)), let xw = (xhi-xlo) in (sumw d / xw, dup (uncert d / xw)))
        uncert d = sqrt(sumw2 d)


point :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) =>
         ((Double, (Double, Double)), (Double, (Double, Double))) -> Diagram b
point ((x0, (xlo, xhi)), (y0, (ylo, yhi))) = hrule (xlo+xhi) # translate (r2 (x0 + (xhi-xlo)/2.0, y0))
                                             `atop`
                                             vrule (ylo+yhi) # translate (r2 (x0, y0 + (yhi-ylo)/2.0))
