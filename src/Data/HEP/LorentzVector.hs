{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.HEP.LorentzVector where

import           Control.Lens

import           Data.Semigroup

import           Data.Foldable  (maximumBy)
import           Data.Ord       (comparing)
import           Data.Serialize
import           GHC.Generics   (Generic)


-- TODO
-- this formulation could be quite slow in some cases.

data PtEtaPhiE =
    PtEtaPhiE
        { __pt  :: {-# UNPACK #-} !Double
        , __eta :: {-# UNPACK #-} !Double
        , __phi :: {-# UNPACK #-} !Double
        , __e   :: {-# UNPACK #-} !Double
        } deriving (Show, Generic)

makeLenses ''PtEtaPhiE
instance Serialize PtEtaPhiE where


data XYZT =
    XYZT
        { __x :: {-# UNPACK #-} !Double
        , __y :: {-# UNPACK #-} !Double
        , __z :: {-# UNPACK #-} !Double
        , __t :: {-# UNPACK #-} !Double
        }
    deriving (Show, Generic)

makeLenses ''XYZT
instance Serialize XYZT where


-- PtEtaPhiE and XYZT are of course isomorphic.
xyztPepe :: Iso' XYZT PtEtaPhiE
xyztPepe = iso f g
    where f (XYZT x y z t) =
            let pt = sqrt(x**2 + y**2)
            in PtEtaPhiE
                pt
                (negate . log . tan . (/2) $ atan2 pt z)
                (atan2 y x)
                t

          g (PtEtaPhiE pt eta phi e) =
            XYZT
                (pt*cos phi)
                (pt*sin phi)
                (pt*sinh eta)
                e


class HasLorentzVector a where
    toPtEtaPhiE :: Lens' a PtEtaPhiE
    toPtEtaPhiE = toXYZT . xyztPepe

    toXYZT :: Lens' a XYZT
    toXYZT = toPtEtaPhiE . from xyztPepe


instance HasLorentzVector XYZT where
    toXYZT = iso id id

instance HasLorentzVector PtEtaPhiE where
    toPtEtaPhiE = iso id id


lvX :: HasLorentzVector a => Lens' a Double
lvX = toXYZT . _x
lvY :: HasLorentzVector a => Lens' a Double
lvY = toXYZT . _y
lvZ :: HasLorentzVector a => Lens' a Double
lvZ = toXYZT . _z
lvT :: HasLorentzVector a => Lens' a Double
lvT = toXYZT . _t

lvPt :: HasLorentzVector a => Lens' a Double
lvPt = toPtEtaPhiE . _pt
lvEta :: HasLorentzVector a => Lens' a Double
lvEta = toPtEtaPhiE . _eta
lvPhi :: HasLorentzVector a => Lens' a Double
lvPhi = toPtEtaPhiE . _phi
lvE :: HasLorentzVector a => Lens' a Double
lvE = toPtEtaPhiE . _e

lvRap :: HasLorentzVector a => Getter a Double
lvRap = to $ \v -> let e = view lvE v
                       pz = view lvPz v
                   in  0.5 * log ((e + pz) / (e - pz))


lvPx :: HasLorentzVector a => Lens' a Double
lvPx = lvX
lvPy :: HasLorentzVector a => Lens' a Double
lvPy = lvY
lvPz :: HasLorentzVector a => Lens' a Double
lvPz = lvZ


lvTheta :: HasLorentzVector a => Getter a Double
lvTheta = to $ atan2 <$> view lvPt <*> view lvPz

lvP2 :: HasLorentzVector a => Getter a Double
lvP2 = to $ (+) <$> view lvPt2 <*> view lvZ2

lvPt2 :: HasLorentzVector a => Getter a Double
lvPt2 = to $ (+) <$> view lvX2 <*> view lvY2

squareL :: Num b => Getter a b -> Getter a b
squareL l = to $ \x -> let y = view l x in y*y

lvX2 :: HasLorentzVector a => Getter a Double
lvX2 = squareL lvX

lvY2 :: HasLorentzVector a => Getter a Double
lvY2 = squareL lvY

lvZ2 :: HasLorentzVector a => Getter a Double
lvZ2 = squareL lvZ

lvT2 :: HasLorentzVector a => Getter a Double
lvT2 = squareL lvT

lvM :: HasLorentzVector a => Getter a Double
lvM = to $ sqrt . view lvM2

lvM2 :: HasLorentzVector a => Getter a Double
lvM2 = to $ (-) <$> view lvE2 <*> view lvP2

lvE2 :: HasLorentzVector a => Getter a Double
lvE2 = lvT2


-- flip only the 3 vector of a LorentzVector
lvNegate :: HasLorentzVector v => v -> v
lvNegate = over toXYZT $
    \(XYZT x y z t) -> XYZT (negate x) (negate y) (negate z) t


lvDot :: (HasLorentzVector v, HasLorentzVector v') => v -> v' -> Double
lvDot a b = view lvX a * view lvX b +
            view lvY a * view lvY b +
            view lvZ a * view lvZ b -
            view lvT a * view lvT b


twoPi :: Double
twoPi = 2*pi

lvDPhi :: (HasLorentzVector v, HasLorentzVector v') => v -> v' -> Double
lvDPhi v v' = f $ view lvPhi v - view lvPhi v'
    where f x | x < (-pi) = f (x+twoPi)
              | x >= pi    = f (x-twoPi)
              | otherwise = x


lvDRap :: (HasLorentzVector a, HasLorentzVector b) => a -> b -> Double
lvDRap v v' = view lvRap v - view lvRap v'

lvDEta :: (HasLorentzVector a, HasLorentzVector b) => a -> b -> Double
lvDEta v v' = view lvEta v - view lvEta v'


lvDREta :: (HasLorentzVector a, HasLorentzVector b) => a -> b -> Double
lvDREta v v' = sqrt $ dEta2 + dPhi2
    where
        dPhi2 = let dp = lvDPhi v v' in dp*dp
        dEta2 = let de = lvDEta v v' in de*de

lvDRRap :: (HasLorentzVector a, HasLorentzVector b) => a -> b -> Double
lvDRRap v v' = sqrt $ dRap2 + dPhi2
    where
        dPhi2 = let dp = lvDPhi v v' in dp*dp
        dRap2 = let de = lvDRap v v' in de*de


lvAbsEta :: HasLorentzVector a => Getter a Double
lvAbsEta = lvEta . to abs


withIso2 :: (a -> a -> a) -> Iso' b a -> b -> b -> b
withIso2 f i x y = view (from i) $ f (view i x) (view i y)

-- all LorentzVectors are monoids under addition
instance Semigroup PtEtaPhiE where
    (<>) = withIso2 (<>) (from xyztPepe)

instance Monoid PtEtaPhiE where
    mempty = PtEtaPhiE 0 0 0 0
    mappend = (<>)

instance Semigroup XYZT where
    XYZT x y z t <> XYZT x' y' z' t' =
        XYZT
            (x + x')
            (y + y')
            (z + z')
            (t + t')

instance Monoid XYZT where
    mempty = XYZT 0 0 0 0
    mappend = (<>)


leading :: (Foldable f, HasLorentzVector a) => f a -> Maybe a
leading fa | null fa   = Nothing
           | otherwise = Just (maximumBy (comparing $ view lvPt) fa)
