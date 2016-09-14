{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
 - taken from
 - http://pdg.lbl.gov/2002/montecarlorpp.pdf
 -}

module Data.HEP.PID
    ( PID, HasPID(..)
    , hasPID, abspid
    , anti
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
    , chargedLeptons, neutrinos
    , leptons
    , downTypeQuarks, upTypeQuarks
    , quarks, partons
    , weakBosons, ewBosons
    , hasQuark
    , hasBottomQuark, hasCharmQuark
    , ofType, ofClass
    , isQuark, isChargedLepton, isNeutrino
    , isLepton, isTau
    , isHadron, isMeson, isDiquark, isBaryon
    ) where

import Control.Lens

import Data.Set as S

-- TODO
-- Integer?
newtype PID = PID Int
        deriving (Eq, Ord, Enum, Show, Num, Real, Integral)

class HasPID hp where
    pid :: Lens' hp PID

hasPID :: HasPID hp => hp -> PID -> Bool
hasPID part p = p == view pid part

instance HasPID PID where
    pid = id

abspid :: HasPID hp => Getter hp PID
abspid = pid . to abs


type PIDSet = Set PID

anti :: PIDSet -> PIDSet
anti = S.map negate

electron, eNeutrino, muon, mNeutrino, tau, tNeutrino :: PIDSet
down, up, strange, charm, bottom, top :: PIDSet
gluon, photon, gamma, zBoson, wPlus, wMinus :: PIDSet
higgs, hPlus, hMinus :: PIDSet


electron = [11]
eNeutrino = [12]

muon = [13]
mNeutrino = [14]

tau = [15]
tNeutrino = [16]

down = [1]
up = [2]
strange = [3]
charm = [4]
bottom = [5]
top = [6]

gluon = [21]
photon = [22]
gamma = photon
zBoson = [23]
wPlus = [24]
wMinus = [-24]

higgs = [25]

hPlus = [37]
hMinus = [-37]

chargedLeptons, neutrinos, leptons :: PIDSet
chargedLeptons =
    unions
        [ electron, anti electron
        , muon, anti muon
        , tau, anti tau
        ]

neutrinos =
    unions
        [ eNeutrino, anti eNeutrino
        , mNeutrino, anti mNeutrino
        , tNeutrino, anti tNeutrino
        ]

leptons = chargedLeptons `union` neutrinos

downTypeQuarks, upTypeQuarks, quarks, partons :: PIDSet
downTypeQuarks =
    unions
        [ down, anti down
        , strange, anti strange
        , bottom, anti bottom
        ]

upTypeQuarks =
    unions
        [ up, anti up
        , charm, anti charm
        , top, anti charm
        ]


quarks = downTypeQuarks `union` upTypeQuarks

partons = gluon `union` quarks

weakBosons, ewBosons :: PIDSet
weakBosons = unions [zBoson, wPlus, wMinus]

ewBosons = photon `union` weakBosons


type PIDClass = PID -> Bool

diquark, hadron, meson, baryon :: PIDClass
diquark p = 1000 < p && p < 7000
hadron p = nq2 p > 0 && nq3 p > 0
meson p = hadron p && nq1 p == 0
baryon p = hadron p && nq1 p > 0

-- TODO
-- this could be faster as Int?
digit :: Integral a => a -> a -> a
digit x i = div (abs x) (10^i) `mod` 10


nJ, nq3, nq2, nq1, nL, nr, n :: PID -> PID
nJ = flip digit 0
nq3 = flip digit 1
nq2 = flip digit 2
nq1 = flip digit 3
nL = flip digit 4
nr = flip digit 5
n = flip digit 6


hasQuark :: PID -> PID -> Bool
hasQuark p q = (hadron p || diquark p) && elem q (sequenceA [nq1, nq2, nq3] p :: [PID])

hasBottomQuark, hasCharmQuark :: PIDClass
hasBottomQuark = flip hasQuark 5
hasCharmQuark = flip hasQuark 4


ofType :: HasPID hp => hp -> PIDSet -> Bool
ofType p = member (view pid p)

typeOf :: HasPID hp => PIDSet -> hp -> Bool
typeOf = flip ofType

ofClass :: HasPID hp => hp -> PIDClass -> Bool
ofClass p pc = pc (view pid p)

classOf :: HasPID hp => PIDClass -> hp -> Bool
classOf = flip ofClass


isQuark, isChargedLepton, isNeutrino, isLepton :: HasPID hp => hp -> Bool
isQuark = typeOf quarks
isChargedLepton = typeOf chargedLeptons
isNeutrino = typeOf neutrinos
isLepton = typeOf leptons
isTau = typeOf tau . view abspid

isHadron, isMeson, isBaryon, isDiquark, isTau :: HasPID hp => hp -> Bool
isHadron = classOf hadron
isMeson = classOf meson
isDiquark = classOf diquark 
isBaryon = classOf baryon
