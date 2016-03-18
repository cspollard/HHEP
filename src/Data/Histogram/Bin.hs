{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies #-}

module Data.Histogram.Bin where

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)
import Control.Arrow ((***))

import Data.Histogram.Dimension

class Bin b where
    type BinValue b
    idx :: b -> BinValue b -> Int
    nbins :: b -> Int

class Bin b => IntervalBin b where
    binEdges :: b -> [(BinValue b, BinValue b)]


-- constant interval binning in one dimension
data Bin1D a = Bin1D Int (a, a) deriving (Generic, Show, Eq)

instance (Serialize a) => Serialize (Bin1D a) where

instance Functor Bin1D where
    f `fmap` Bin1D n (mn, mx) = Bin1D n (f mn, f mx)

instance RealFrac a => Bin (Bin1D a) where
    type BinValue (Bin1D a) = a

    Bin1D n (mn, mx) `idx` x | x < mn = 0
                             | x > mx = n+1
                             | otherwise = floor (fromIntegral n * (x - mn) / (mx - mn)) + 1

    nbins (Bin1D n _) = n


instance RealFrac a => IntervalBin (Bin1D a) where
    binEdges (Bin1D n (xmin, xmax)) = take n $ iterate (\(_, b) -> (b, b+step)) (xmin, xmin+step)
        where
            step = (xmax - xmin) / (fromIntegral n)


instance (Bin a, Bin b) => Bin (a :. b) where
    type BinValue (a :. b) = (BinValue a :. BinValue b)

    (bx :. by) `idx` (x, y) = (bx `idx` x) + (nbins bx + 1) * (by `idx` y)

    nbins (bx :. by) = (nbins bx + 1) * (nbins by + 1)


instance (IntervalBin a, IntervalBin b) => IntervalBin (a :. b) where
    binEdges (bx :. by) = [((x0, y0), (x, y)) | (x0, x) <- binEdges bx, (y0, y) <- binEdges by]


-- convenience types
type Bin2D a = Bin1D a :. Bin1D a
type Bin3D a = Bin2D a :. Bin1D a
