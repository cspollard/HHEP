{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies #-}

module Data.Histogram.Bin where

-- TODO
-- it would be good to encode the # and size of bins in the type.
-- somehow...

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)
import Data.TypeList

class Bin b where
    type BinValue b
    idx :: b -> BinValue b -> Int
    -- NB: this must include under/overflow!
    nbins :: b -> Int

class Bin b => IntervalBin b where
    binEdges :: b -> [(BinValue b, BinValue b)]


-- constant interval binning in one dimension
data Bin0D a = Bin0D deriving (Generic, Show, Eq)

data ConstBin a = ConstBin Int (a, a) deriving (Generic, Show, Eq)

data ArbBin a = ArbBin Int [a] deriving (Generic, Show, Eq)

instance (Serialize a) => Serialize (Bin0D a) where
instance (Serialize a) => Serialize (ConstBin a) where
instance (Serialize a) => Serialize (ArbBin a) where

instance Functor Bin0D where
    _ `fmap` Bin0D = Bin0D

instance Functor ConstBin where
    f `fmap` ConstBin n (mn, mx) = ConstBin n (f mn, f mx)

instance Functor ArbBin where
    f `fmap` ArbBin n xs = ArbBin n $ fmap f xs


instance Bin (Bin0D a) where
    type BinValue (Bin0D a) = Z
    idx _ = const 0
    nbins = const 1


instance RealFrac a => Bin (ConstBin a) where
    type BinValue (ConstBin a) = a

    ConstBin n (mn, mx) `idx` x | x < mn = 0
                                | x > mx = n+1
                                | otherwise = floor (fromIntegral n * (x - mn) / (mx - mn)) + 1

    nbins (ConstBin n _) = n+2


-- TODO
-- this might be slow
instance Ord a => Bin (ArbBin a) where
    type BinValue (ArbBin a) = a

    ArbBin _ edges `idx` x = go 0 edges x
        where go n [] _ = n
              go n (x0:xs) x' | x' < x0 = n
                              | otherwise = go (n+1) xs x'

    nbins (ArbBin n _) = n+2


instance IntervalBin (Bin0D a) where
    binEdges = const [(Z, Z)]


instance RealFrac a => IntervalBin (ConstBin a) where
    binEdges (ConstBin n (xmin, xmax)) = take n $ iterate (\(_, b) -> (b, b+step)) (xmin, xmin+step)
        where step = (xmax - xmin) / fromIntegral n


instance Ord a => IntervalBin (ArbBin a) where
    binEdges (ArbBin _ xs) = go xs
        where go (x:x':xs') = (x, x') : go (x':xs')
              go _          = []


instance (Bin a, Bin b) => Bin (a :. b) where
    type BinValue (a :. b) = (BinValue a :. BinValue b)

    (bx :. by) `idx` (x :. y) = nbins by * (bx `idx` x) + (by `idx` y)

    nbins (bx :. by) = nbins bx * nbins by


instance (IntervalBin a, IntervalBin b) => IntervalBin (a :. b) where
    binEdges (bx :. by) = [(x0 :. y0, x :. y) | (x0, x) <- binEdges bx, (y0, y) <- binEdges by]


constBin1D :: Int -> (a, a) -> ConstBin1D a
constBin1D n = (Bin0D :.) . ConstBin n

arbBin1D :: [a] -> ArbBin1D a
arbBin1D xs = Bin0D :. ArbBin (length xs - 1) xs

-- convenience types
type ConstBin1D a = Bin0D a :. ConstBin a
type ConstBin2D a = ConstBin1D a :. ConstBin a
type ConstBin3D a = ConstBin2D a :. ConstBin a

type ArbBin1D a = Bin0D a :. ArbBin a
type ArbBin2D a = ArbBin1D a :. ArbBin a
type ArbBin3D a = ArbBin2D a :. ArbBin a
