{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Data.Histogram.Distribution (
                                     DistW(..), DistWX(..)
                                   , Dist0D, Dist1D
                                   , ScaleW(..)
                                   , module Data.TypeList
                                   ) where

-- N-dimensional distributions

import Data.Monoid
import Data.TypeList


-- stores enough info to get the 2nd moment of a distribution
-- could use laziness to get the higher moments, but seems inefficient
data DistW a = DistW { sumw :: !(Sum a), sumw2 :: !(Sum a), nentries :: !(Sum Int) }
data DistWX a = DistWX { sumwx :: !(Sum a), sumwx2 :: !(Sum a) }


-- TODO
-- should this be generalized to keep wxy terms?
-- how do I store additional moments?
-- w, w2, wx, wx2, wy, wy2, wxy, etc
-- look into GHC.TypeLits
-- -XDataKinds

type Dist0D a = DistW a
type Dist1D a = Dist0D a :. DistWX a

instance Num a => Monoid (DistW a) where
    mempty = DistW mempty mempty mempty
    DistW sw sw2 ne `mappend` DistW sw' sw2' ne' =
            DistW (sw <> sw') (sw2 <> sw2') (ne <> ne')

instance Num a => Monoid (DistWX a) where
    mempty = DistWX mempty mempty
    DistWX swx swx2 `mappend` DistWX swx' swx2' =
            DistWX (swx <> swx') (swx2 <> swx2')


instance Num a => Distribution (DistW a) where
    type X (DistW a) = a
    d `fill` w = d <> fill mempty w

instance Num a => Distribution (DistWX a) where
    type X (DistWX a) = a
    d `fill` x = d <> fill mempty x

instance (Distribution a, Distribution b, ScaleW b, X a ~ W b) => Distribution (a :. b) where
    type X (a :. b) = X a :. X b
    (dw :. dxs) `fill` (w :. xs) = fill dw w :. (fill dxs xs `scaleW` w)



class ScaleW s where
    type W s :: *
    scaleW :: s -> W s -> s

class Distribution d where
    type X d :: *
    fill :: d -> X d -> d

class Distribution s => ScaleX s where
    scaleX :: s -> X s -> s


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
    DistWX (Sum swx) (Sum swx2) `scaleW` w =
            DistWX (Sum $ swx `scaleW` w) (Sum $ swx2 `scaleW` w)


instance Num a => ScaleX (DistWX a) where
    -- possibly quite inefficient...
    DistWX (Sum swx) (Sum swx2) `scaleX` x =
            DistWX (Sum $ swx*x) (Sum $ swx2*x*x)


-- the last two are necessary for (a :. b) to be a Distribution
-- TODO
-- I'm not confident that DistW a :. DistWX a is going to work
-- correctly here.
instance (ScaleX a, ScaleX b, ScaleW b, X a ~ W b) => ScaleX (a :. b) where
    (dx :. dy) `scaleX` (x :. y) = (dx `scaleX` x) :. (dy `scaleX` y)


-- scale the appropriate dimension 
-- scaleX :: Num a => (Z :. n :. n') -> (Dist0 a :. d) -> a -> (Dist0 a :. d)
