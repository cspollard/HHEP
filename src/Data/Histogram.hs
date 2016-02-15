{-# LANGUAGE DeriveGeneric #-}

module Data.Histogram where

import Data.Foldable
import Data.Vector (Vector(..), (//), (!))
import qualified Data.Vector as V

import Data.Binary (Binary(..))
import GHC.Generics (Generic)


-- very simple histogram implementation
data Histogram a b = Histogram Int (b, b) (Vector a) deriving Generic

instance (Binary a) => Binary (Vector a) where
    put = put . V.toList
    get = V.fromList <$> get

instance (Binary a, Binary b) => Binary (Histogram a b) where


histogram :: (RealFloat b) => Int -> (b, b) -> a -> Histogram a b
histogram n range init = Histogram n range (V.replicate (n+2) init)


fillOne :: (RealFloat b) => (c -> a -> a) -> Histogram a b -> (b, c) -> Histogram a b
fillOne f (Histogram n (mn, mx) v) (x, w) = Histogram n (mn, mx) $ v // [(ix, f w $ v ! ix)]
    where ix = floor (fromIntegral n * (x - mn) / (mx - mn)) + 1


data Builder a b = Builder { built :: b, build :: a -> Builder a b }

premap :: (a -> a') -> Builder a' b -> Builder a b
premap f (Builder x g) = Builder x (fmap (premap f) (g . f))

instance Functor (Builder a) where
    -- f :: b -> c
    -- x :: b
    -- g :: a -> Builder a b
    fmap f (Builder x g) = Builder (f x) (fmap (fmap f) g)

builder :: (b -> a -> b) -> b -> Builder a b
builder f x = Builder x (\y -> builder f (f x y))

foldlBuilder :: Foldable f => f a -> Builder a b -> Builder a b
foldlBuilder r ys = foldl build ys r


{-
-- TODO
-- -- need to figure out the right way to do this.
-- roll :: (b -> a -> b) -> b -> a -> (b, a -> b)
-- roll f x y = let x' = f x y in (x', f x') 
--
-- foldRoll :: b -> t a -> (b, a -> b)
--

data Builder e h = Builder h (e -> h)

instance Functor (Builder e) where
    fmap f (Builder h g) = Builder (f h) (fmap f g)

instance Applicative (Builder e) where
    pure x = Builder x (const x)
    (Builder f g) <*> (Builder x y) = Builder (f x) (\s -> g s (y s))

instance Monad (Builder e) where
    (Builder h f) >>= g = g h
-}

