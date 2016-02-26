{-# LANGUAGE DeriveGeneric #-}

module Data.Histogram where

import Control.Arrow ((&&&))

import Data.Foldable
import Data.Vector (Vector(..), indexM, (!), (//), modify)
import Data.Vector.Mutable (write)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Data.Functor.Identity (runIdentity)

import Data.Histogram.Bin


-- very simple histogram implementation
data Histogram b a = Histogram b !(Vector a) deriving (Generic, Show)

instance Functor (Histogram b) where
    f `fmap` Histogram x v = Histogram x $ f `fmap` v

binmap :: (b -> c) -> Histogram b a -> Histogram c a
f `binmap` Histogram b v = Histogram (f b) v

-- TODO
-- logspace

instance Foldable (Histogram b) where
    foldr f b (Histogram _ v) = foldr f b v


instance (Binary a) => Binary (Vector a) where
    put = put . V.toList
    get = V.fromList <$> get

instance (Binary a, Binary b) => Binary (Histogram b a) where

histogram :: (Bin b) => b -> a -> Histogram b a
histogram bins init = Histogram bins (V.replicate (nbins bins + 2) init)

indexStrict :: Vector b -> Int -> b
indexStrict v i = runIdentity $ v `indexM` i

-- a version of modify that forces evaluation of the vector element at
-- ix
modify' :: (b -> b) -> Int -> Vector b -> Vector b
modify' f ix = modify $ \v -> do
                            y <- MV.read v ix
                            write v ix $! f y

fillOne :: (Bin b) => (a -> c -> a) -> Histogram b a -> (BinValue b, c) -> Histogram b a
fillOne f (Histogram b v) (x, w) = Histogram b $ modify' (flip f w) (idx b x) v



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
foldBuilder b@(Builder x f) = Builder x (\y -> foldBuilder $ feed' b y)

histBuilder :: (Bin b) => (a -> c -> a) -> Histogram b a -> Builder (BinValue b, c) (Histogram b a)
histBuilder f = builder (fillOne f)

integral :: Monoid a => Histogram b a -> a
integral = fold

underflow :: Histogram b a -> a
underflow (Histogram _ v) = v ! 0

overflow :: Bin b => Histogram b a -> a
overflow (Histogram b v) = v ! (nbins b + 1)

toTuples :: RealFrac b => Histogram (Bin1D b) a -> [((b, b), a)]
toTuples (Histogram bins v) = zip (binEdges bins) $ map (v !) [1..n]
    where
        n = nbins bins

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
