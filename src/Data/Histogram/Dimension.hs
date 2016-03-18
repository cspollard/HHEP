{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Data.Histogram.Dimension where

-- taken directly from the Repa library
infixl 3 :.
data head :. tail = !head :. !tail
