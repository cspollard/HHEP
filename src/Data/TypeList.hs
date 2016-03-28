{-# LANGUAGE DeriveGeneric, TypeOperators #-}

module Data.TypeList where

import GHC.Generics
import Data.Serialize
import Data.Semigroup

-- taken directly from the Repa library

-- the null type
data Z = Z deriving (Generic, Show, Eq)


instance Monoid Z where
    mempty = Z
    mappend = const


infixl 3 :.
data head :. tail = !head :. !tail
        deriving (Generic, Show, Eq)


instance (Semigroup a, Semigroup b) => Semigroup (a :. b) where
    (x :. y) <> (x' :. y') =
            (x <> x') :. (y <> y')

instance (Monoid a, Monoid b) => Monoid (a :. b) where
    mempty = mempty :. mempty
    (x :. y) `mappend` (x' :. y') =
            (x `mappend` x') :. (y `mappend` y')


instance (Serialize a, Serialize b) => Serialize (a :. b) where
