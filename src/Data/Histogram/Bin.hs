{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Data.Histogram.Bin where

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)
import Control.Arrow ((***))

class Bin b where
    type BinValue b
    idx :: b -> BinValue b -> Int
    nbins :: b -> Int

data Bin1D a = Bin1D Int (a, a) deriving (Generic, Show, Eq)

instance RealFrac a => Bin (Bin1D a) where
    type BinValue (Bin1D a) = a

    Bin1D n (mn, mx) `idx` x | x < mn = 0
                             | x > mx = n+1
                             | otherwise = floor (fromIntegral n * (x - mn) / (mx - mn)) + 1

    nbins (Bin1D n _) = n


instance Functor Bin1D where
    f `fmap` Bin1D n (mn, mx) = Bin1D n (f mn, f mx)

instance (Serialize a) => Serialize (Bin1D a) where


data Bin2D a b = Bin2D (Bin1D a) (Bin1D b) deriving (Generic, Show)

instance (RealFrac a, RealFrac b) => Bin (Bin2D a b) where
    type BinValue (Bin2D a b) = (a, b)

    Bin2D bx by `idx` (x, y) = (bx `idx` x) + (nbins bx + 1) * (by `idx` y)

    nbins (Bin2D bx by) = (nbins bx + 1) * (nbins by + 1)


instance (Serialize a, Serialize b) => Serialize (Bin2D a b) where


class Bin b => IntervalBin b where
    binEdges :: b -> [(BinValue b, BinValue b)]


instance RealFrac a => IntervalBin (Bin1D a) where
    binEdges (Bin1D n (xmin, xmax)) = take n $ iterate (\(_, b) -> (b, b+step)) (xmin, xmin+step)
        where
            step = (xmax - xmin) / (fromIntegral n)


instance (RealFrac a, RealFrac b) => IntervalBin (Bin2D a b) where
    binEdges (Bin2D bx by) = [((x0, y0), (x, y)) | (x0, x) <- binEdges bx, (y0, y) <- binEdges by]
