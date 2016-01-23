{-# LANGUAGE TupleSections #-}

-- based largely on fastjet

module Data.HEP.Jet where

import Data.List.Ordered
import Data.HEP.LorentzVector

-- PseudoJet () -> no associated info
data PseudoJet a =
                 -- a jet: the distance between child pseudojets, its
                 -- 4 momentum, and its child pseudojets
                   PJet Double PtEtaPhiE (PseudoJet a, PseudoJet a)
                 -- a constituent: its distance to the beam, its 4
                 -- momentum, and its associated data
                 | PJConst Double PtEtaPhiE a
                 deriving Show


instance Eq a => Eq (PseudoJet a) where
    (PJet _ _ (p1, p2)) == (PJet _ _ (q1, q2)) = p1 == q1 && p2 == q2
    (PJConst _ _ p) == (PJConst _ _ q) = p == q
    _ == _ = False

instance HasLorentzVector (PseudoJet a) where
    lv (PJet _ p _) = fromLV p
    lv (PJConst _ p _) = fromLV p

-- TODO
-- Ord necessary?


pjdist :: PseudoJet a -> Double
pjdist (PJet d _ _) = d
pjdist (PJConst d _ _) = d


-- TODO
-- surely this already exists somewhere
-- probably not optimal
combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x:xs) = map (x,) xs ++ combinations xs

-- a JetAlg takes f
type JetAlg a = [a] -> [PseudoJet a]

{-
runJetAlg :: (HasLorentzVector a, Eq a, LorentzVector b) =>
             (b -> b -> Double) ->
             (b -> Double) ->
             JetAlg a
runJetAlg dij diB as =
    where
        dbeams = foldr (insertBagBy pjdist) [] $ [PJConst (diB p) (lv p) p | p <- as]
        -- TODO
        -- this double counts.
        dists = foldr (insertBagBy pjdist) dbeams $ \(p, q) -> PJet (dij p q) (lv p <> lv q) | p <- as, q <- as]
        -}
