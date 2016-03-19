{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Data.Histogram.Distribution where

-- N-dimensional distributions

import Data.Monoid
import Data.TypeList


-- stores enough info to get the 2nd moment of a distribution
-- could use laziness to get the higher moments, but seems inefficient
data DistW a = DistW { sumw :: !(Sum a), sumw2 :: !(Sum a), nentries :: !(Sum Int) }
data DistWX a = DistWX { sumwx :: !(Sum a), sumwx2 :: !(Sum a) }

type Dist0D a = DistW a
type Dist1D a = Dist0D a :. DistWX a
type Dist2D a = Dist1D a :. DistWX a
type Dist3D a = Dist2D a :. DistWX a


-- TODO
-- convert (w, x) pair to a distribution

instance Num a => Monoid (DistW a) where
    mempty = DistW mempty mempty mempty
    DistW sw sw2 ne `mappend` DistW sw' sw2' ne' =
            DistW (sw <> sw') (sw2 <> sw2') (ne <> ne')

instance Num a => Monoid (DistWX a) where
    mempty = DistWX mempty mempty
    DistWX swx swx2 `mappend` DistWX swx' swx2' =
            DistWX (swx <> swx') (swx2 <> swx2')

instance (Monoid a, Monoid b) => Monoid (a :. b) where
    mempty = mempty :. mempty
    (x :. y) `mappend` (x' :. y') =
            (x <> x') :. (y <> y')


class ScaleW s where
    type W s :: *
    scaleW :: s -> W s -> s


-- this is annoying but I guess necessary...
instance ScaleW Double where
    type W Double = Double 
    scaleW = (*)


instance ScaleW Int where
    type W Int = Int 
    scaleW = (*)


instance ScaleW a => ScaleW (DistW a) where
    type W (DistW a) = W a

    -- possibly quite inefficient...
    DistW (Sum sw) (Sum sw2) ne `scaleW` w =
            DistW (Sum $ sw `scaleW` w) (Sum $ sw2 `scaleW` w `scaleW` w) ne


instance ScaleW a => ScaleW (DistWX a) where
    type W (DistWX a) = W a
    -- possibly quite inefficient...
    DistWX (Sum swx) (Sum swx2) `scaleW` w =
            DistWX (Sum $ swx `scaleW` w) (Sum $ swx2 `scaleW` w `scaleW` w)


instance (ScaleW a, ScaleW b, w ~ W a, w ~ W b) => ScaleW (a :. b) where
    type W (a :. b) = W a
    (x :. y) `scaleW` w = (x `scaleW` w) :. (y `scaleW` w)


-- scale the appropriate dimension 
-- scaleX :: Num a => (Z :. n :. n') -> (Dist0 a :. d) -> a -> (Dist0 a :. d)
