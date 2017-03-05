{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}

{-
 - taken from
 - http://pdg.lbl.gov/2002/montecarlorpp.pdf
 -}

-- TODO
-- everything really should be INLINABLE here.

module Data.HEP.PID
    ( PID, HasPID(..)
    , PIDClass
    , isA, ofClass, classOf
    , hasPID, abspid
    , anti
    , union, unions, intersection, intersections
    , electron, eNeutrino
    , muon, mNeutrino
    , tau, tNeutrino
    , down, up
    , strange, charm
    , bottom, top
    , gluon
    , photon, gamma
    , zBoson, wPlus, wMinus
    , higgs, hPlus, hMinus
    , chargedLepton, neutrino
    , lepton
    , downTypeQuark, upTypeQuark
    , quark, parton
    , weakBoson, ewBoson
    , bottomHadron, charmHadron
    , isQuark, isChargedLepton, isNeutrino, isLepton
    , isHadron, isMeson, isBaryon, isDiquark, isTau
    ) where

import           Control.Lens
import qualified Data.Set     as S

newtype PID = PID Int
        deriving (Eq, Ord, Enum, Show, Num, Real, Integral)

class HasPID a where
    pid :: Lens' a PID

hasPID :: HasPID a => a -> PID -> Bool
hasPID part p = p == view pid part

instance HasPID PID where
    pid = id

abspid :: HasPID a => Getter a PID
abspid = pid . to abs


-- need this datatype so that we can union sets before applying function...
data PIDClass where
  PIDFunc :: (PID -> Bool) -> PIDClass
  PIDSet :: S.Set PID -> PIDClass

classOf :: HasPID a => PIDClass -> a -> Bool
classOf (PIDFunc f) = views pid f
classOf (PIDSet s)  = views pid (`S.member` s)

ofClass, isA :: HasPID a => a -> PIDClass -> Bool
ofClass = flip classOf
isA = ofClass

union :: PIDClass -> PIDClass -> PIDClass
union (PIDSet s) (PIDSet s') = PIDSet $ s `S.union` s'
union c c'                   = PIDFunc $ (||) <$> classOf c <*> classOf c'

intersection :: PIDClass -> PIDClass -> PIDClass
intersection (PIDSet s) (PIDSet s') = PIDSet $ s `S.intersection` s'
intersection c c'                   = PIDFunc $ (&&) <$> classOf c <*> classOf c'

newtype Union = U { unU :: PIDClass }
inU2 f (U x) (U y) = U (f x y)

instance Monoid Union where
  mempty = U $ PIDSet S.empty
  mappend = inU2 union

newtype Intersection = I { unI :: PIDClass }
inI2 f (I x) (I y) = I (f x y)

instance Monoid Intersection where
  mempty = I $ PIDFunc (const True)
  mappend = inI2 intersection


unions :: [PIDClass] -> PIDClass
unions = unU . mconcat . fmap U

intersections :: [PIDClass] -> PIDClass
intersections = unI . mconcat . fmap I

anti :: PIDClass -> PIDClass
anti (PIDFunc f) = PIDFunc $ f . negate
anti (PIDSet s)  = PIDSet $ S.map negate s

electron, eNeutrino, muon, mNeutrino, tau, tNeutrino :: PIDClass
down, up, strange, charm, bottom, top :: PIDClass
gluon, photon, gamma, zBoson, wPlus, wMinus :: PIDClass
higgs, hPlus, hMinus :: PIDClass


electron = PIDSet [11]
eNeutrino = PIDSet [12]

muon = PIDSet [13]
mNeutrino = PIDSet [14]

tau = PIDSet [15]
tNeutrino = PIDSet [16]

down = PIDSet [1]
up = PIDSet [2]
strange = PIDSet [3]
charm = PIDSet [4]
bottom = PIDSet [5]
top = PIDSet [6]

gluon = PIDSet [21]
photon = PIDSet [22]
gamma = photon
zBoson = PIDSet [23]
wPlus = PIDSet [24]
wMinus = PIDSet [-24]

higgs = PIDSet [25]

hPlus = PIDSet [37]
hMinus = PIDSet [-37]

chargedLepton, neutrino, lepton :: PIDClass
chargedLepton =
  unions
    [ electron, anti electron
    , muon, anti muon
    , tau, anti tau
    ]

neutrino =
  unions
    [ eNeutrino, anti eNeutrino
    , mNeutrino, anti mNeutrino
    , tNeutrino, anti tNeutrino
    ]

lepton = chargedLepton `union` neutrino

downTypeQuark, upTypeQuark, quark, parton :: PIDClass
downTypeQuark =
  unions
    [ down, anti down
    , strange, anti strange
    , bottom, anti bottom
    ]

upTypeQuark =
  unions
    [ up, anti up
    , charm, anti charm
    , top, anti charm
    ]


quark = downTypeQuark `union` upTypeQuark
parton = gluon `union` quark

weakBoson, ewBoson :: PIDClass
weakBoson = unions [zBoson, wPlus, wMinus]
ewBoson = photon `union` weakBoson

diquark, hadron, meson, baryon :: PIDClass
diquark = PIDFunc $ \p -> 1000 < p && p < 7000
hadron = PIDFunc $ \p -> nq2 p > 0 && nq3 p > 0
meson = hadron `intersection` PIDFunc ((== 0) . nq1)
baryon = hadron `intersection` PIDFunc ((> 0) . nq1)

digit :: Integral a => a -> a -> a
digit x i = div (abs x) (10^i) `mod` 10
{-# INLINABLE digit #-}

pidDig i = views pid (`digit` i)
{-# INLINABLE pidDig #-}

nJ, nq3, nq2, nq1, nL, nr, n :: HasPID a => a -> PID
nJ = pidDig 0
nq3 = pidDig 1
nq2 = pidDig 2
nq1 = pidDig 3
nL = pidDig 4
nr = pidDig 5
n = pidDig 6
{-# INLINABLE nJ #-}
{-# INLINABLE nq3 #-}
{-# INLINABLE nq2 #-}
{-# INLINABLE nq1 #-}
{-# INLINABLE nL #-}
{-# INLINABLE nr #-}
{-# INLINABLE n #-}


quarkIn :: PID -> PIDClass
quarkIn q = (hadron `union` diquark) `intersection`
  PIDFunc (\p -> q `elem` (sequenceA [nq1, nq2, nq3] p :: [PID]))


bottomHadron, charmHadron :: PIDClass
bottomHadron = quarkIn 5
charmHadron = quarkIn 4


isQuark, isChargedLepton, isNeutrino, isLepton :: HasPID a => a -> Bool
isQuark = classOf quark
isChargedLepton = classOf chargedLepton
isNeutrino = classOf neutrino
isLepton = classOf lepton
isTau = classOf tau . view abspid

isHadron, isMeson, isBaryon, isDiquark, isTau :: HasPID hp => hp -> Bool
isHadron = classOf hadron
isMeson = classOf meson
isDiquark = classOf diquark
isBaryon = classOf baryon
