{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Data.Histogram ( Histogram(..)
                      , binmap
                      , histogram
                      , fillOneF , fillOne
                      , histBuilderF, histBuilder
                      , integral , underflow , overflow
                      , hadd
                      , toTuples
                      , Histo1D
                      , module Data.TypeList
                      , module Data.Histogram.Bin
                      , module Data.Histogram.Distribution
                      , module Data.Builder
                      ) where



import Data.Foldable
import Data.Vector (Vector(..), indexM, (!), (//), modify)
import Data.Vector.Mutable (write)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)

import Data.Functor.Identity (runIdentity)

import Data.Serialize.Vector

import Data.Monoid ((<>))

import Data.TypeList
import Data.Builder

import Data.Histogram.Bin
import Data.Histogram.Distribution

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


instance ScaleW a => ScaleW (Histogram b a) where
    type W (Histogram b a) = W a
    h `scaleW` w = (`scaleW` w) `fmap` h


histogram :: Bin b => b -> a -> Histogram b a
histogram bins init = Histogram bins (V.replicate (nbins bins + 2) init)


-- a version of modify that forces evaluation of the vector element at
-- ix
modify' :: (b -> b) -> Int -> Vector b -> Vector b
modify' f ix = modify $ \v -> do
                            y <- MV.read v ix
                            write v ix $! f y


-- fill one item in a histogram with a combining function
fillOneF :: (Bin b) => (a -> c -> a) -> Histogram b a -> (BinValue b, c) -> Histogram b a
fillOneF f (Histogram b v) (x, w) = Histogram b $ modify' (flip f w) (idx b x) v


-- fill one item in a histogram with a combining function
fillOne :: (Bin b, Monoid a) => Histogram b a -> (BinValue b, a) -> Histogram b a
fillOne = fillOneF (<>)


histBuilderF :: (Bin b) => (a -> c -> a) -> Histogram b a -> Builder (BinValue b, c) (Histogram b a)
histBuilderF f = builder (fillOneF f)

histBuilder :: (Bin b, Monoid a) => Histogram b a -> Builder (BinValue b, a) (Histogram b a)
histBuilder = builder fillOne


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
-- type Histo2D = Histogram (Bin2D Double) (Dist2D Double)
-- type Histo3D = Histogram (Bin3D Double) (Dist3D Double)
