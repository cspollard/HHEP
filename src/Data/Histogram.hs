{-# LANGUAGE DeriveGeneric, TypeFamilies, TypeOperators #-}


module Data.Histogram( Histogram(..)
                      , histogram, distBuilder
                      , binmap
                      , integral, underflow, overflow
                      , hadd, toTuples
                      , Histo1D, module X) where

import Data.TypeList as X
import Data.Builder as X
import Data.Histogram.Bin as X
import Data.Histogram.Distribution as X

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


instance ScaleW a => ScaleW (Histogram b a) where
    type W (Histogram b a) = W a
    h `scaleW` w = (`scaleW` w) `fmap` h

instance (Bin b, Distribution a, BinValue b ~ X a) => Distribution (Histogram b a) where
    type X (Histogram b a) = X a
    fill (Histogram b v) w xs = Histogram b $ modify' (\d -> fill d w xs) (idx b xs) v


distBuilder :: Distribution a => a -> Builder (W a, X a) a
distBuilder = builder (\d (w, x) -> fill d w x)


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
