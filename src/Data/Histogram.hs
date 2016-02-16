{-# LANGUAGE DeriveGeneric #-}

module Data.Histogram where

import Data.Foldable
import Data.Vector (Vector(..), (//), (!))
import qualified Data.Vector as V

import Data.Binary (Binary(..))
import GHC.Generics (Generic)


-- very simple histogram implementation
data Histogram b a = Histogram Int (b, b) (Vector a) deriving Generic

instance Functor (Histogram b) where
    f `fmap` Histogram x y v = Histogram x y $ f `fmap` v

instance Foldable (Histogram b) where
    foldr f b (Histogram x y v) = foldr f b v


instance (Binary a) => Binary (Vector a) where
    put = put . V.toList
    get = V.fromList <$> get

instance (Binary a, Binary b) => Binary (Histogram b a) where


histogram :: (RealFloat b) => Int -> (b, b) -> a -> Histogram b a
histogram n range init = Histogram n range (V.replicate (n+2) init)


fillOne :: (RealFloat b) => (a -> c -> a) -> Histogram b a -> (b, c) -> Histogram b a
fillOne f (Histogram n (mn, mx) v) (x, w) = Histogram n (mn, mx) $ v // [(ix, f (v ! ix) w)]
    where ix = floor (fromIntegral n * (x - mn) / (mx - mn)) + 1


-- TODO
-- is this easier with just (b -> a -> b) functions?
-- unclear how to have Functor and Applicative instances that way.
-- but: are they really necessary?

data Builder a b = Builder { built :: b, build :: a -> Builder a b }

premap :: (a -> a') -> Builder a' b -> Builder a b
premap f (Builder x g) = Builder x (fmap (premap f) (g . f))

(<<-) = flip premap

instance Functor (Builder a) where
    -- f :: b -> c
    -- x :: b
    -- g :: a -> Builder a b
    fmap f (Builder x g) = Builder (f x) (\y -> fmap f (g y))

instance Applicative (Builder a) where
    pure x = builder const x
    -- f :: b -> c
    -- g :: a -> Builder a (b -> c)
    -- x :: b
    -- y :: a -> Builder a b
    Builder f g <*> Builder x y = Builder (f x) (\w -> g w <*> y w)


builder :: (b -> a -> b) -> b -> Builder a b
builder f x = Builder x (\y -> builder f (f x y))

feedl :: Foldable f => Builder a b -> f a -> Builder a b
feedl = foldl build

feedr :: Foldable f => Builder a b -> f a -> Builder a b
feedr = foldr (flip build)

foldrBuilder :: Foldable f => Builder a b -> Builder (f a) b
foldrBuilder (Builder x g) = let f y z = built (feedr (Builder y g) z) in builder f x

foldlBuilder :: Foldable f => Builder a b -> Builder (f a) b
foldlBuilder (Builder x g) = let f y z = built (feedl (Builder y g) z) in builder f x

histBuilder :: (RealFloat b) => (a -> c -> a) -> Histogram b a -> Builder (b, c) (Histogram b a)
histBuilder f = builder (fillOne f)

integral :: Monoid a => Histogram b a -> a
integral = fold

underflow :: Histogram b a -> a
underflow (Histogram _ _ v) = v ! 0

overflow :: Histogram b a -> a
overflow (Histogram n _ v) = v ! (n+1)

toTuples :: (RealFloat b) => Histogram b a -> [((b, b), a)]
toTuples (Histogram n (xmin, xmax) v) = map (\ix -> (f ix, v ! ix)) [1..n]
    where
        step = (xmax - xmin) / (fromIntegral n)
        f i = let y = xmin + (fromIntegral i)*step in (y, y+step)

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

