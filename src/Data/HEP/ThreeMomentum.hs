{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HEP.ThreeMomentum where

import           Control.Lens
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

inner :: Num a => XYZ a -> XYZ a -> a
inner (XYZ x y z) (XYZ x' y' z') = (x*x') + (y*y') + (z*z')

cross :: Num a => XYZ a -> XYZ a -> XYZ a
cross (XYZ x y z) (XYZ x' y' z') =
  XYZ (y*z' - z*y') (z*x' - x*z') (x*y' - x'*y)

modulus :: Num a => XYZ a -> a
modulus xyz = inner xyz xyz
