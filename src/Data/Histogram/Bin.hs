{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies #-}

module Data.Histogram.Bin where

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)
import Data.TypeList

class Bin b where
    type BinValue b
    idx :: b -> BinValue b -> Int
    nbins :: b -> Int

class Bin b => IntervalBin b where
    binEdges :: b -> [(BinValue b, BinValue b)]


-- constant interval binning in one dimension
data Bin0D a = Bin0D deriving (Generic, Show, Eq)

data ConstBin a = ConstBin Int (a, a) deriving (Generic, Show, Eq)

instance (Serialize a) => Serialize (Bin0D a) where
instance (Serialize a) => Serialize (ConstBin a) where

instance Functor Bin0D where
    _ `fmap` Bin0D = Bin0D

instance Functor ConstBin where
    f `fmap` ConstBin n (mn, mx) = ConstBin n (f mn, f mx)


instance Bin (Bin0D a) where
    type BinValue (Bin0D a) = Z
    idx _ = const 0
    nbins = const 0

instance RealFrac a => Bin (ConstBin a) where
    type BinValue (ConstBin a) = a

    ConstBin n (mn, mx) `idx` x | x < mn = 0
                             | x > mx = n+1
                             | otherwise = floor (fromIntegral n * (x - mn) / (mx - mn)) + 1

    nbins (ConstBin n _) = n


instance IntervalBin (Bin0D a) where
    binEdges = const [(Z, Z)]

instance RealFrac a => IntervalBin (ConstBin a) where
    binEdges (ConstBin n (xmin, xmax)) = take n $ iterate (\(_, b) -> (b, b+step)) (xmin, xmin+step)
        where
            step = (xmax - xmin) / fromIntegral n


instance (Bin a, Bin b) => Bin (a :. b) where
    type BinValue (a :. b) = (BinValue a :. BinValue b)

    (bx :. by) `idx` (x :. y) = (bx `idx` x) + (nbins bx + 1) * (by `idx` y)

    nbins (bx :. by) = (nbins bx + 1) * (nbins by + 1)


instance (IntervalBin a, IntervalBin b) => IntervalBin (a :. b) where
    binEdges (bx :. by) = [(x0 :. y0, x :. y) | (x0, x) <- binEdges bx, (y0, y) <- binEdges by]


bin1D :: Int -> (a, a) -> Bin1D a
bin1D n = (Bin0D :.) . ConstBin n

-- convenience types
type Bin1D a = Bin0D a :. ConstBin a
type Bin2D a = Bin1D a :. ConstBin a
type Bin3D a = Bin2D a :. ConstBin a
