{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies #-}

module Data.Histogram.Distribution (
                                     Dist0(..), DistWX(..)
                                   , Dist0D, Dist1D
                                   , ScaleW(..), Distribution(..)
                                   , module Data.TypeList
                                   ) where

import GHC.Generics
import Data.Serialize
import Data.TypeList


-- stores enough info to get the 2nd moment of a distribution
-- could use laziness to get the higher moments, but seems inefficient
data Dist0 a = Dist0 { sumw :: !a, sumw2 :: !a, nentries :: !Int }
    deriving (Generic, Show, Eq, Ord)

data DistWX a = DistWX { sumwx :: !a, sumwx2 :: !a }
    deriving (Generic, Show, Eq, Ord)

instance Serialize a => Serialize (Dist0 a) where
instance Serialize a => Serialize (DistWX a) where


-- TODO
-- should this be generalized to keep wxy terms?
-- how do I store additional moments?
-- w, w2, wx, wx2, wy, wy2, wxy, etc
-- look into GHC.TypeLits
-- -XDataKinds

type Dist0D a = Dist0 a
type Dist1D a = Dist0D a :. DistWX a

instance Num a => Monoid (Dist0 a) where
    mempty = Dist0 0 0 0
    Dist0 sw sw2 ne `mappend` Dist0 sw' sw2' ne' =
            Dist0 (sw + sw') (sw2 + sw2') (ne + ne')

instance Num a => Monoid (DistWX a) where
    mempty = DistWX 0 0
    DistWX swx swx2 `mappend` DistWX swx' swx2' =
            DistWX (swx + swx') (swx2 + swx2')

class ScaleW s where
    type W s :: *
    scaleW :: s -> W s -> s


instance Num a => ScaleW (Dist0 a) where
    type W (Dist0 a) = a
    Dist0 sw sw2 ne `scaleW` w =
            Dist0 (sw*w) (sw2*w*w) ne

instance Num a => ScaleW (DistWX a) where
    type W (DistWX a) = a
    DistWX swx swx2 `scaleW` w =
            DistWX (swx*w) (swx2*w)

instance (ScaleW a, ScaleW b, W a ~ W b) => ScaleW (a :. b) where
    type W (a :. b) = W a
    (dxs :. dy) `scaleW` w = (dxs `scaleW` w) :. (dy `scaleW` w)


class ScaleW d => Distribution d where
    type X d :: *
    fill :: d -> W d -> X d -> d


instance Num a => Distribution (Dist0 a) where
    type X (Dist0 a) = Z
    fill (Dist0 sw sw2 n) w _ = Dist0 (sw+w) (sw2+w*w) (n+1)

instance Num a => Distribution (DistWX a) where
    type X (DistWX a) = a
    fill (DistWX swx swx2) w x = DistWX (swx+w*x) (swx2+w*x*x)

instance (Distribution a, Distribution b, W a ~ W b) => Distribution (a :. b) where
    type X (a :. b) = X a :. X b
    fill (dxs :. dx) w (xs :. x) = fill dxs w xs :. fill dx w x



{-
class Distribution s => ScaleX s where
    scaleX :: s -> X s -> s
instance Num a => ScaleX (DistWX a) where
    -- possibly quite inefficient...
    DistWX (Sum swx) (Sum swx2) `scaleX` x =
            DistWX (Sum $ swx*x) (Sum $ swx2*x*x)


-- the last two are necessary for (a :. b) to be a Distribution
-- TODO
-- I'm not confident that Dist0 a :. DistWX a is going to work
-- correctly here.
instance (ScaleX a, ScaleX b, ScaleW b, X a ~ W b) => ScaleX (a :. b) where
    (dx :. dy) `scaleX` (x :. y) = (dx `scaleX` x) :. (dy `scaleX` y)
-}
