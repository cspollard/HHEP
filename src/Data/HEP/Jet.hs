{-# LANGUAGE TupleSections #-}

-- based largely on fastjet

module Data.HEP.Jet where

import Data.List.Ordered
import Data.HEP.LorentzVector


data PseudoJet a = PseudoJet PtEtaPhiE (PseudoJet a, PseudoJet a)
                 | Ghost PtEtaPhiE a
                 | Constituent PtEtaPhiE a


instance HasLorentzVector (PseudoJet a) where
    lv (PseudoJet v _ ) = v
    lv (Ghost v _ ) = v


pseudoJet :: (HasLorentzVector a) => a -> PseudoJet a
ghost x = Ghost (lv x) x

ghost :: (HasLorentzVector a) => a -> Double -> PseudoJet a
ghost vec ghostScale = Ghost (lvScale ghostScale (lv x)) x

cluster :: Ord b => PJDist a b -> JetAlg a
cluster d xs = undefined


clusterStep :: [(Double, Int, Int)] -> IntMap (PseudoJet a) -> [PseudoJet a] -> [PseudoJet a]
-- if no pjs to cluster
-- or if only beam
-- return jets
clusterStep [] _ js = js
clusterStep ds _ js = js

-- TODO
-- this could be much faster.
clusterStep d (pj@(PJNode _ _ (pj', pj''):pjs) js = 
    where
        pjs' = filter (\p -> not $ (p `hasConst` pj') || (p `hasConst` pj'')) pjs


instance Eq a => Eq (PseudoJet a) where
    (PJet _ _ (p1, p2)) == (PJet _ _ (q1, q2)) = p1 == q1 && p2 == q2
    (PJConst _ _ p) == (PJConst _ _ q) = p == q
    PJBeam == PJBeam = True
    _ == _ = False

instance HasLorentzVector (PseudoJet a) where
    lv (PJet _ p _) = fromLV p
    lv (PJConst _ p _) = fromLV p

-- TODO
-- Ord necessary?

hasPJ :: PseudoJet a -> PseudoJet a -> Bool
hasPJ p (PJet _ _ (q, q')) = (p == q) || (p == q')
hasPJ p _ = error "PJConst has no subjets."

pjdist :: PseudoJet a -> Double
pjdist (PJet d _ _) = d
pjdist (PJConst d _ _) = d


-- TODO
-- surely this already exists somewhere
-- probably not optimal
combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x:xs) = map (x,) xs ++ combinations xs

type JetAlg a = [a] -> [PseudoJet a]


runJetAlg :: (HasLorentzVector a, Eq a, LorentzVector b) =>
             (b -> b -> Double) ->
             (b -> Double) ->
             JetAlg a
runJetAlg dij diB as =
    where
        dbeams = foldr (insertBagBy pjdist) [] $ [PJConst (diB p) (lv p) p | p <- as]
        dists = foldr (insertBagBy pjdist) dbeams $ map (\(p, q) -> PJet (dij p q) (lv p <> lv q) (p, q)) (combinations as)

        f ps' [] = ps'
        f ps' (p:ps) = case p of
                        PJet _ _ _ _ -> 
                        PConst _ _ _ _ -> f (p:ps') (filter (not . hasPJ p) ps)
