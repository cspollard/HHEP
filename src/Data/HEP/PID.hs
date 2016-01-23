{-
 - taken from
 - http://pdg.lbl.gov/2002/montecarlorpp.pdf
 -}

-- TODO
-- a lot of this needs to be hidden.
module Data.HEP.PID where

import Data.Set

-- TODO
-- Integer?
type PID = Int

type PIDSet = Set PID

-- TODO
-- tagged union PIDClass?

class HasPID hp where
    pid :: hp -> PID

abspid :: HasPID hp => hp -> PID
abspid = abs . pid


electron, eNeutrino, muon, mNeutrino, tau, tNeutrino :: Int

down, up, strange, charm, bottom, top :: Int

gluon, photon, gamma, z, wplus, wminus :: Int

h, hplus, hminus :: Int


electron = 11
eNeutrino = 12

muon = 13
mNeutrino = 14

tau = 15
tNeutrino = 16

down = 1
up = 2
strange = 3
charm = 4
bottom = 5
top = 6

gluon = 21
photon = 22
gamma = photon
z = 23
wplus = 24
wminus = -wplus

-- TODO
-- BSM higgses...
-- a0?
-- h0?

h = 25

hplus = 37
hminus = -hplus

chargedLeptons = fromList [electron, -electron,
                           muon, -muon,
                           tau, -tau]

neutrinos = fromList [eNeutrino, -eNeutrino,
                      mNeutrino, -mNeutrino,
                      tNeutrino, -tNeutrino]

leptons = union chargedLeptons neutrinos



downTypeQuarks = fromList [down, -down,
                           strange, -strange,
                           bottom, -bottom]

upTypeQuarks = fromList [up, -up,
                         charm, -charm,
                         top, -top]


quarks = union downTypeQuarks upTypeQuarks

partons = insert gluon quarks

weakBosons = fromList [z, wplus, wminus]

ewBosons = insert photon weakBosons


diquark p = 1000 < p && p < 7000
hadron p = nq2 p > 0 && nq3 p > 0
meson p = hadron p && nq1 p == 0
baryon p = hadron p && nq1 p > 0

-- TODO
-- this could be faster as Int?
digit :: (Integral a) => a -> a -> a
digit x n = div (abs x) (10^n) `mod` 10

nJ = flip digit 0
nq3 = flip digit 1
nq2 = flip digit 2
nq1 = flip digit 3
nL = flip digit 4
nr = flip digit 5
n = flip digit 6

hasQuark p q = (hadron p || diquark p) && (or . fmap ( (==) q . ($ p) ) $ [nq1, nq2, nq3])

hasBottomQuark = flip hasQuark bottom
hasCharmQuark = flip hasQuark charm

ofType :: HasPID hp => hp -> PIDSet -> Bool
ofType p = member (pid p)

isType :: HasPID hp => hp -> PID -> Bool
isType = (==) . pid

isQuark, isChargedLepton, isNeutrino, isLepton :: HasPID hp => hp -> Bool
isQuark p = p `ofType` quarks
isChargedLepton p = p `ofType` chargedLeptons
isNeutrino p = p `ofType` neutrinos
isLepton p = p `ofType` leptons

isHadron, isMeson, isBaryon, isDiquark, isTau :: HasPID hp => hp -> Bool
isDiquark = diquark . pid
isHadron = hadron . pid
isMeson = meson . pid
isBaryon = baryon . pid
isTau p = abspid p == tau

isPID :: HasPID hp => PID -> hp -> Bool
isPID p part = p == pid part

isPIDs :: HasPID hp => PIDSet -> hp -> Bool
isPIDs ps part = pid part `member` ps
