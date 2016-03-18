{-# LANGUAGE DeriveGeneric #-}

module Data.Histogram where

import Control.Arrow ((&&&))

import Data.Foldable
import Data.Vector (Vector(..), indexM, (!), (//), modify)
import Data.Vector.Mutable (write)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)

import Data.Functor.Identity (runIdentity)

import Data.Builder
import Data.Histogram.Bin
import Data.Histogram.Dimension
import Data.Histogram.Distribution

import Data.Serialize.Vector

import Data.Monoid ((<>))

-- very simple histogram implementation
data Histogram b a = Histogram b !(Vector a) deriving (Generic, Show)

instance Functor (Histogram b) where
    f `fmap` Histogram x v = Histogram x $ f `fmap` v

-- map a function over bins
binmap :: (b -> c) -> Histogram b a -> Histogram c a
f `binmap` Histogram b v = Histogram (f b) v

-- TODO
-- logspace

instance Foldable (Histogram b) where
    foldr f b (Histogram _ v) = foldr f b v


instance (Serialize a, Serialize b) => Serialize (Histogram b a) where


histogram :: Bin b => b -> a -> Histogram b a
histogram bins init = Histogram bins (V.replicate (nbins bins + 2) init)

-- a version of modify that forces evaluation of the vector element at
-- ix
modify' :: (b -> b) -> Int -> Vector b -> Vector b
modify' f ix = modify $ \v -> do
                            y <- MV.read v ix
                            write v ix $! f y

-- fill one item in a histogram with a combining function
fillOne :: (Bin b) => (a -> c -> a) -> Histogram b a -> (BinValue b, c) -> Histogram b a
fillOne f (Histogram b v) (x, w) = Histogram b $ modify' (flip f w) (idx b x) v


histBuilder :: (Bin b) => (a -> c -> a) -> Histogram b a -> Builder (BinValue b, c) (Histogram b a)
histBuilder f = builder (fillOne f)


integral :: Monoid a => Histogram b a -> a
integral = fold

underflow :: Histogram b a -> a
underflow (Histogram _ v) = v ! 0

overflow :: Bin b => Histogram b a -> a
overflow (Histogram b v) = v ! (nbins b + 1)


hadd :: (Eq b, Monoid a) => Histogram b a -> Histogram b a -> Maybe (Histogram b a)
hadd (Histogram b v) (Histogram b' v')
        | b == b' = Just $ Histogram b (V.zipWith (<>) v v')
        | otherwise = Nothing


-- this does not include overflow.
toTuples :: IntervalBin b => Histogram b a -> [((BinValue b, BinValue b), a)]
toTuples (Histogram bins v) = zip (binEdges bins) $ map (v !) [1..n]
    where
        n = nbins bins


-- convenience types
type Histo1D = Histogram (Bin1D Double) (Dist1D Double)
type Histo2D = Histogram (Bin2D Double) (Dist2D Double)
