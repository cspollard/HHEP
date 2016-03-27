{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts #-}

module Graphics.Histo where

import Data.TypeList
import Diagrams.Prelude
import Data.Histogram


type PtErr2D = ((Double, (Double, Double)), (Double, (Double, Double)))

type Graph2D = [PtErr2D]

dup :: a -> (a, a)
dup x = (x, x)

histToGraph :: Histo1D -> Graph2D
histToGraph = map toPoint . toTuples
    where
        toPoint ((Z :. xlo, Z :. xhi), d :. _) = let x0 = (xlo+xhi)/2.0 in
                                    ((x0, (xhi-x0, x0-xlo)), let xw = (xhi-xlo) in (sumw d / xw, dup (uncert d / xw)))
        uncert d = sqrt(sumw2 d)



drawGraph :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) =>
             Graph2D -> Diagram b
drawGraph = mconcat . map drawPoint


drawPoint :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) => PtErr2D -> Diagram b
drawPoint ((x0, (xlo, xhi)), (y0, (ylo, yhi))) = hrule (xlo+xhi) # translate (r2 (x0 + (xhi-xlo)/2.0, y0))
                                             `atop`
                                             vrule (ylo+yhi) # translate (r2 (x0, y0 + (yhi-ylo)/2.0))


xAxis :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) => Diagram b -> Diagram b
xAxis d = case extentX d of
               Nothing       -> hrule 1
               Just (lo, hi) -> hrule (hi-lo) # translateX ((hi+lo)/2)

yAxis :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) => Diagram b -> Diagram b
yAxis d = case extentY d of
               Nothing       -> vrule 1
               Just (lo, hi) -> vrule (hi-lo) # translateY ((hi+lo)/2)
