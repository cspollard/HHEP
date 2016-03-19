{-# LANGUAGE TypeOperators #-}

module Data.TypeList where

import Data.Monoid

-- taken directly from the Repa library
infixl 3 :.

data head :. tail = !head :. !tail

data Z = Z

instance Monoid Z where
    mempty = Z
    mappend = const

instance (Monoid a, Monoid b) => Monoid (a :. b) where
    mempty = mempty :. mempty
    (x :. y) `mappend` (x' :. y') =
            (x <> x') :. (y <> y')
