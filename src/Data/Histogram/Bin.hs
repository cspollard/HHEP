{-# LANGUAGE DeriveGeneric
           , TypeFamilies #-}

module Data.Histogram.Bin where

import Data.Binary (Binary(..))
import GHC.Generics (Generic)
import Control.Arrow ((***))

class Bin b where
    type BinValue b
    idx :: b -> BinValue b -> Int
    nbins :: b -> Int

data Bin1D a = Bin1D Int (a, a) deriving (Generic, Show)

instance (RealFrac a) => Bin (Bin1D a) where
    type BinValue (Bin1D a) = a

    idx (Bin1D n (mn, mx)) x | x < mn = 0
                             | x > mx = n+1
                             | otherwise = floor (fromIntegral n * (x - mn) / (mx - mn)) + 1

    nbins (Bin1D n _) = n


instance Functor Bin1D where
    f `fmap` Bin1D n (mn, mx) = Bin1D n (f mn, f mx)

instance (Binary a) => Binary (Bin1D a) where

-- TODO
-- really only for a particular kind of bin...
binEdges :: Fractional a => Bin1D a -> [(a, a)]
binEdges (Bin1D n (xmin, xmax)) = take n $ iterate (\(_, b) -> (b, b+step)) (xmin, xmin+step)
        where
            step = (xmax - xmin) / (fromIntegral n)
