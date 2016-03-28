{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts #-}

module Graphics.Histo where

import Diagrams.Prelude hiding (diff)
import Diagrams.TwoD.Text
import Numeric (showGFloat)

import Data.Maybe (fromMaybe)

import Data.TypeList
import Data.Histogram

type PtErr2D = ((Double, (Double, Double)), (Double, (Double, Double)))

type Graph2D = [PtErr2D]

histToGraph :: Histo1D -> Graph2D
histToGraph = map toPoint . toTuples
    where
        toPoint ((Z :. xlo, Z :. xhi), d :. _) = let x0 = (xlo+xhi)/2.0 in
                                    ((x0, (xhi-x0, x0-xlo)), let xw = (xhi-xlo) in (sumw d / xw, dup (uncert d / xw)))
        uncert d = sqrt(sumw2 d)


forceDimensions :: (V b ~ V2, N b ~ Double)
                => (Double, Double) -> Diagram b -> (Transformation V2 Double, Diagram b)
-- forceDimensions :: (Additive v, R2 v, R2 (V t), Transformable t, Enveloped t, V t ~ V2)
                -- => (N t, N t) -> t -> (Transformation v (N t), t)
forceDimensions (w', h') d = let w = width d
                                 h = height d
                                 sx = w'/w
                                 sy = h'/h
                             in (inv (scalingX sx <> scalingY sy), d # scaleX sx # scaleY sy)


drawGraph :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
          => Graph2D -> Diagram b
drawGraph = mconcat . map drawPoint


drawPoint :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) => PtErr2D -> Diagram b
drawPoint ((x0, (xlo, xhi)), (y0, (ylo, yhi))) = hline (x0-xlo) (x0+xhi) # translateY y0
                                                 `atop`
                                                 vline (y0-ylo) (y0+yhi) # translateX x0



-- addAxes :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b), Renderable (Text Double) b)
        -- => Diagram b -> Transformation V2 Double -> Diagram b
addAxes :: (TrailLike (QDiagram b (V b) (N b) Any), Renderable (Path V2 Double) b, Renderable (Text Double) b, V b ~ V2, N b ~ Double)
        => Transformation V2 Double -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
addAxes t d = let (xlo, xhi) = fromMaybe (0, 1) $ extentX d
                  (ylo, yhi) = fromMaybe (0, 1) $ extentY d
                  (xlo', ylo') = unp2 . papply t $ mkP2 xlo ylo
                  (xhi', yhi') = unp2 . papply t $ mkP2 xhi yhi
                  xaxispts = map (flip mkP2 ylo') $ axisTicks xlo' xhi'
                  yaxispts = map (mkP2 xlo') $ axisTicks ylo' yhi'
                  tinv = inv t
              in mconcat [ hline xlo xhi # translateY ylo `atop` mconcat [labTickX xap # translate (r2 . unp2 $ papply tinv xap) | xap <- xaxispts]
                         , vline ylo yhi # translateX xlo `atop` mconcat [labTickY yap # translate (r2 . unp2 $ papply tinv yap) | yap <- yaxispts]
                         , d
                         ]


-- TODO
-- could use powers of 2, 10, 25, and 50
-- determine the approrpriate tick spacing for a given scale
tickSpacing :: Double -> Double
tickSpacing s = 10.0 ^^ ((floor :: Double -> Int) . logBase 10 . (/1.3)$ s)


axisTicks :: Double -> Double -> [Double]
axisTicks lo hi = let d = hi-lo
                      s = tickSpacing d
                      strt = (fromIntegral $ ceiling (lo/s)) * s
                  in fromToBy strt hi s


fromToBy :: Double -> Double -> Double -> [Double]
fromToBy strt stp dist = takeWhile (<= stp) $ iterate (+dist) strt


-- generate a set of points with a starting location and a diff vector
iterP2 :: (Num (N a), Transformable a) => a -> Vn a -> [a]
iterP2 xi dx = iterate (translate dx) xi

fromToByX :: (Num n, Ord n) => n -> n -> n -> n -> [P2 n]
fromToByX y xi xf dx = takeWhile ((<= xf) . fst . unp2) $ iterP2 (mkP2 xi y) (mkR2 dx 0)

fromToByY :: (Num n, Ord n) => n -> n -> n -> n -> [P2 n]
fromToByY x yi yf dy = takeWhile ((<= yf) . snd . unp2) $ iterP2 (mkP2 x yi) (mkR2 0 dy)


-- labTickX, minTickX, labTickY, minTickY :: P2 (N b) -> Diagram b
labTickX, minTickX, labTickY, minTickY
    :: (TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b)
    => P2 n -> QDiagram b V2 n Any
labTickX p = text (showGFloat (Just 1) (fst $ unp2 p) "") # translateY (-0.05) # fontSizeL 0.05
             `atop` vrule 0.05

minTickX = const $ vrule 0.025

labTickY p = text (showGFloat (Just 1) (snd $ unp2 p) "") # translateX (-0.075) # fontSizeL 0.05
             `atop` hrule 0.05

minTickY = const $ hrule 0.025


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


-- TODO
-- some utility that certainly exists elsewhere...
diff :: Num a => a -> a -> a
diff = (-)

avg :: Fractional a => a -> a -> a
avg x y = (x+y) / 2

dup :: a -> (a, a)
dup x = (x, x)
