{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HEP.ThreeMomentum where

import           Control.Lens
import           Data.Semigroup
import           Data.Serialize
import           GHC.Generics   (Generic)

data XYZ a =
  XYZ
  { _vx :: !a
  , _vy :: !a
  , _vz :: !a
  } deriving (Show, Generic, Functor)

makeLenses ''XYZ
instance Serialize a => Serialize (XYZ a) where

instance Num a => Semigroup (XYZ a) where
  XYZ x1 y1 z1 <> XYZ x2 y2 z2 = XYZ (x1+x2) (y1+y2) (z1+z2)

instance Num a => Monoid (XYZ a) where
  mempty = XYZ 0 0 0


inner :: Num a => XYZ a -> XYZ a -> a
inner (XYZ x y z) (XYZ x' y' z') = (x*x') + (y*y') + (z*z')

cross :: Num a => XYZ a -> XYZ a -> XYZ a
cross (XYZ x y z) (XYZ x' y' z') =
  XYZ (y*z' - z*y') (z*x' - x*z') (x*y' - x'*y)

modulus2 :: Num a => XYZ a -> a
modulus2 xyz = inner xyz xyz

modulus :: Floating a => XYZ a -> a
modulus = sqrt . modulus2

diff :: Num a => XYZ a -> XYZ a -> XYZ a
diff (XYZ x1 y1 z1) (XYZ x2 y2 z2) = XYZ (x1-x2) (y1-y2) (z1-z2)
