{-# LANGUAGE TypeOperators, TypeFamilies #-}

module Data.TypeList where

-- taken directly from the Repa library
infixl 3 :.

data head :. tail = !head :. !tail
