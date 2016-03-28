{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts #-}

module Graphics.Histo where

import Diagrams.Prelude hiding (diff)
import Diagrams.TwoD.Text
import Numeric (showGFloat)

import Data.Maybe (fromMaybe)

import Data.TypeList
import Data.Histogram

import Debug.Trace

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


-- TODO
-- there must be a better way to do this...
line :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
     => P2 Double -> P2 Double -> Diagram b
line xyi xyf = let (xi, yi) = unp2 xyi
                   (xf, yf) = unp2 xyf
                in trailLike $ trailFromSegments [straight $ r2 (xf-xi, yf-yi)] `at` xyi

hline :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
      => Double -> Double -> Diagram b
hline xi xf = line (mkP2 xi 0) (mkP2 xf 0)

vline :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
      => Double -> Double -> Diagram b
vline yi yf = line (mkP2 0 yi) (mkP2 0 yf)


drawGraph :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) =>
             Graph2D -> Diagram b
drawGraph = mconcat . map drawPoint . traceShowId


drawPoint :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) => PtErr2D -> Diagram b
drawPoint ((x0, (xlo, xhi)), (y0, (ylo, yhi))) = hline (x0-xlo) (x0+xhi) # translateY y0
                                                 `atop`
                                                 vline (y0-ylo) (y0+yhi) # translateX x0


addAxes :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b), Renderable (Text Double) b)
      => Diagram b -> Diagram b
addAxes d = let (xlo, xhi) = fromMaybe (0, 1) $ extentX d
                (ylo, yhi) = fromMaybe (0, 1) $ extentY d
                xscale = xhi - xlo
                yscale = yhi - ylo
            in mconcat [ d
                       , translateY ylo $ hline xlo xhi `atop` xIterEvery xlo xhi (tickSpacing xscale) (labTickX $ 0.03*yscale)
                       , translateX xlo $ vline ylo yhi `atop` yIterEvery ylo yhi (tickSpacing yscale) (labTickY $ 0.03*xscale)
                       ]


-- determine the approrpriate tick spacing for a given scale
tickSpacing :: Double -> Double
tickSpacing s = traceShowId $ 10.0 ^^ ((floor :: Double -> Int) . logBase 10 . (/4.0) $ s)


-- TODO
-- it's possible for these to return one item too few if strt and stp
-- are separated by an integer multiple of dx/dy.
xIterEvery :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
           => Double -> Double -> Double -> (Double -> Diagram b) -> Diagram b
xIterEvery strt stp dx d = mconcat . fmap (\x -> d x # translateX x) . takeWhile (<= stp) $ iterate (+dx) strt

yIterEvery :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
           => Double -> Double -> Double -> (Double -> Diagram b) -> Diagram b
yIterEvery strt stp dy d = mconcat . fmap (\y -> d y # translateY y) . takeWhile (<= stp) $ iterate (+dy) strt


-- TODO
-- don't like normalized font size here.
labTickX, minTickX, labTickY, minTickY
    :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b), Renderable (Text Double) b)
    => Double -> Double -> Diagram b
labTickX s x = vrule 1.0 # scale s
              `atop` text (showGFloat (Just 1) x "") # translateY (-1*s) # fontSizeN 0.03
minTickX s = const $ vrule 0.5 # scale s

labTickY s y = text (showGFloat (Just 1) y "") # translateX (-1.5*s) # fontSizeN 0.03
               `atop` hrule 1.0 # scale s
minTickY s = const $ hrule 0.5 # scale s


-- TODO
-- some utility that certainly exists elsewhere...
diff :: Num a => a -> a -> a
diff = (-)

avg :: Fractional a => a -> a -> a
avg x y = (x+y) / 2
