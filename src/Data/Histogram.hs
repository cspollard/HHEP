{-# LANGUAGE DeriveGeneric #-}

module Data.Histogram where

import Data.Foldable
import Data.Vector (Vector(..), indexM, (!), (//), modify)
import Data.Vector.Mutable (write)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Data.Functor.Identity (runIdentity)


-- very simple histogram implementation
-- TODO
-- should be strict in vector?
data Histogram b a = Histogram Int (b, b) !(Vector a) deriving (Generic, Show)

instance Functor (Histogram b) where
    f `fmap` Histogram x y v = Histogram x y $ f `fmap` v

-- bimap?
binmap :: (b -> c) -> Histogram b a -> Histogram c a
binmap f (Histogram n (b1, b2) v) = Histogram n (f b1, f b2) v

-- TODO
-- logspace

instance Foldable (Histogram b) where
    foldr f b (Histogram x y v) = foldr f b v


instance (Binary a) => Binary (Vector a) where
    put = put . V.toList
    get = V.fromList <$> get

instance (Binary a, Binary b) => Binary (Histogram b a) where


histogram :: (RealFloat b) => Int -> (b, b) -> a -> Histogram b a
histogram n range init = Histogram n range (V.replicate (n+2) init)

indexStrict :: Vector b -> Int -> b
indexStrict v i = runIdentity $ v `indexM` i

-- a version of modify that forces evaluation of the vector element at
-- ix
modify' :: (b -> b) -> Int -> Vector b -> Vector b
modify' f ix = modify $ \v -> do
                            y <- MV.read v ix
                            write v ix $! f y

fillOne :: (RealFloat b) => (a -> c -> a) -> Histogram b a -> (b, c) -> Histogram b a
fillOne f (Histogram n (mn, mx) v) (x, w) = Histogram n (mn, mx) $ modify' (flip f w) ix v
    where
        ix | x < mn = 0
           | x > mx = n+1
           | otherwise = floor (fromIntegral n * (x - mn) / (mx - mn)) + 1



-- TODO
-- is this easier with just (b -> a -> b) functions?
-- unclear how to have Functor and Applicative instances that way.
-- but: are they really necessary?
-- seems I can do

{-
newtype Folder a b = Folder (b -> a -> b)

instance Functor (Folder a) where
    -- f :: b -> c
    -- g :: b -> a -> b
    -- x :: b
    f `fmap` g = \x -> f `fmap` g x
-}

data Builder a b = Builder { built :: !b, build :: a -> Builder a b }

instance (Show b) => Show (Builder a b) where
    show (Builder b _) = "Builder " ++ show b ++ " , " ++ "..."

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

feed :: Foldable f => Builder a b -> f a -> Builder a b
feed = foldl build

feed' :: Foldable f => Builder a b -> f a -> Builder a b
feed' = foldl' build


foldBuilder :: Foldable f => Builder a b -> Builder (f a) b
foldBuilder (Builder x g) = let f y z = built (feed' (Builder y g) z) in builder f x

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
