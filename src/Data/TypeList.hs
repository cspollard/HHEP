{-# LANGUAGE DeriveGeneric, TypeOperators #-}

module Data.TypeList where

import Data.Monoid
import GHC.Generics
import Data.Serialize

-- taken directly from the Repa library

-- the null type
data Z = Z deriving (Generic, Show, Eq)


instance Monoid Z where
    mempty = Z
    mappend = const

instance (Monoid a, Monoid b) => Monoid (a :. b) where
    mempty = mempty :. mempty
    (x :. y) `mappend` (x' :. y') =
            (x <> x') :. (y <> y')


infixl 3 :.
data head :. tail = !head :. !tail
        deriving (Generic, Show, Eq)

instance (Serialize a, Serialize b) => Serialize (a :. b) where
