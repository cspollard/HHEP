module Data.Builder where

import Data.Foldable

data Builder a b = Builder { built :: !b, build :: a -> Builder a b }

instance (Show b) => Show (Builder a b) where
    show (Builder b _) = "Builder " ++ show b ++ " , " ++ "..."

premap :: (a -> a') -> Builder a' b -> Builder a b
premap f (Builder x g) = Builder x (fmap (premap f) (g . f))

(<<-) = flip premap

instance Functor (Builder a) where
    fmap f (Builder x g) = Builder (f x) (fmap f . g)

instance Applicative (Builder a) where
    pure = builder const
    Builder f g <*> Builder x y = Builder (f x) (\w -> g w <*> y w)

-- instance Monad (Builder a)


builder :: (b -> a -> b) -> b -> Builder a b
builder f x = Builder x (builder f . f x)

feed :: Foldable f => Builder a b -> f a -> Builder a b
feed = foldl build

feed' :: Foldable f => Builder a b -> f a -> Builder a b
feed' = foldl' build

foldBuilder :: Foldable f => Builder a b -> Builder (f a) b
foldBuilder b@(Builder x f) = Builder x (foldBuilder . feed' b)
