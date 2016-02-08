-- 
-- Generate constructive proofs for formulas.
--

module Prover (emptyKB, prove) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Generics.Aliases

import Formula
import Proof

type KnowledgeBase = Map.Map Formula Proof
emptyKB = Map.empty

-- All variable names in all proofs.
varsInKB :: KnowledgeBase -> Set.Set String
varsInKB kb = Set.fromList [v | p <- Map.elems kb, v <- varsOf p ]

-- Variable name that's not present in any proofs of the knowledge base.
newVar :: KnowledgeBase -> String
newVar kb = Maybe.fromMaybe "_" $ List.find notUsed varNames
  where
    notUsed n = Set.notMember n usedNames
    usedNames = varsInKB kb
    varNames = map mkName $ varPairs
    mkName (letter, number) =
        if number == 0 then [letter] else [letter] ++ show number
    varPairs = [(l, n) | n <- [0..], l <- ['a'..'z']]

-- Add formula to knowledge base together with its proof.
assume :: KnowledgeBase -> Formula -> Proof -> KnowledgeBase
assume kb f p =
    if Map.member f kb then kb
    else
        deriveAssumptions $ Map.insert f p kb

deriveAssumptions kb =
    if updatedKB == kb
        then kb
        else
            deriveAssumptions $ updatedKB
  where
    fps = Map.toList kb
    derived1 = [fp | fp1 <- fps,
                     fp <- derive1 kb fp1]
    derived2 = [fp | fp1 <- fps, fp2 <- fps, fp1 /= fp2,
                     fp <- derive2 kb fp1 fp2 ]
    derived3 = [fp | fp1 <- fps, fp2 <- fps, fp3 <- fps,
                     fp1 /= fp2, fp1 /= fp3, fp2 /= fp3,
                     fp <- derive3 kb fp1 fp2 fp3 ]

    derivedFPs = derived1 ++ derived2 ++ derived3
    updatedKB = Map.union kb $ Map.fromList derivedFPs

derive1 kb (a `Conj` b, p) = [(a, appl "fst" p), (b, appl "snd" p)]
derive1 kb (PrnF a, p) = [(a, p)]
derive1 _ _ = []

derive2 kb (a `Impl` b, p) (c, q)
    | a == c    = [(b, Appl p q)]
    | otherwise = []
derive2 _ _ _ = []

derive3 kb (a `Disj` b, p) (c `Impl` d, q) (e `Impl` f, r)
    | a == c && b == e && d == f = [(f, appl3 "cases" p q r)]
    | otherwise                  = []
derive3 _ _ _ _ = []

prove :: KnowledgeBase -> Formula -> Maybe Proof
prove kb f
    | Map.member f kb  = Just $ kb Map.! f
    | otherwise        = prove' kb f

prove' :: KnowledgeBase -> Formula -> Maybe Proof
prove' kb (Impl a b) = do
    pb <- prove kb' b
    return $ Lmbd varName (Just a) pb
  where
    varName = newVar kb
    kb' = assume kb a (Var varName)

prove' kb (Disj a b) =
    orElse pa pb
  where
    pa = do
        pa' <- prove kb a
        return $ appl "inl" pa'
    pb = do
        pb' <- prove kb b
        return $ appl "inr" pb'

prove' kb (Conj a b) = do
    pa' <- prove kb a
    pb' <- prove kb b
    return $ Pair pa' pb'

prove' kb (PrnF a) = prove' kb a

prove' _ _ = Nothing
