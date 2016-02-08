-- 
-- Generate constructive proofs for formulas.
--

module Prover where

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

prove :: KnowledgeBase -> Formula -> Maybe Proof
prove kb f
    | Map.member f kb  = Just $ kb Map.! f
    | otherwise        = prove' kb f

prove' :: KnowledgeBase -> Formula -> Maybe Proof
prove' kb (Impl a b) = do
    pb <- prove kb' b
    return $ Lmbd var (Just a) pb
  where
    var = newVar kb
    kb' = if Map.member a kb then kb else Map.insert a (Var var) kb

prove' kb (Disj a b) =
    orElse pa pb
  where
    pa = do
        pa' <- prove kb a
        return $ Appl (Var "inl") pa'
    pb = do
        pb' <- prove kb b
        return $ Appl (Var "inr") pb'

prove' kb (Conj a b) = do
    pa' <- prove kb a
    pb' <- prove kb b
    return $ Pair pa' pb'

prove' _ _ = Nothing
