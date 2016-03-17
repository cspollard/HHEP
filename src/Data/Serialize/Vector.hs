module Data.Serialize.Vector where

import Data.Serialize

import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad (mapM_)

instance (Serialize a) => Serialize (Vector a) where
    put v = put (V.length v) >> mapM_ put v
    get = get >>= flip V.replicateM get

