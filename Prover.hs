-- 
-- Generate constructive proofs for formulas.
--

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad
import Data.Generics.Aliases

import Formula
import Proof

-- Knowledge base.
type KB = Map.Map Formula Proof
emptyKB = Map.empty

-- All variable names in all proofs.
varsInKB :: KB -> Set.Set String
varsInKB kb = Set.fromList [v | p <- Map.elems kb, v <- varsOf p ]

-- Variable name that's not present in kb.
newVar :: KB -> String
newVar kb = Maybe.fromMaybe "_" $ List.find notUsed varNames
  where
    notUsed n = Set.notMember n usedNames
    usedNames = varsInKB kb
    varNames = map mkName $ varPairs
    mkName (letter, number) =
        if number == 0 then [letter] else [letter] ++ show number
    varPairs = [(l, n) | n <- [0..], l <- ['a'..'z']]

prove :: KB -> Formula -> Maybe Proof
prove kb f
    | Map.member f kb  = Just $ kb Map.! f
    | otherwise        = prove' kb f

prove' :: KB -> Formula -> Maybe Proof
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

printKB kb = forM_ (Map.toList kb) printOne
  where
    printOne (a, b) = putStrLn $ "  " ++ show a ++ " with proof: " ++ show b

printProof kb f = do
    putStrLn ("proving " ++ show f)
    unless (Map.null kb) $ do
        putStrLn ("assuming:")
        printKB kb
    printPr $ prove kb f
    putStrLn ""
  where
    printPr Nothing = putStrLn "failed"
    printPr (Just p) = putStrLn $ ">>> " ++ show p

printProofE f = printProof emptyKB f

pr = Conj (Atom "A") (Atom "B")
cc = Impl (Atom "A") (Atom "B")
f1 = Impl (PrnF pr) cc
p1 = Lmbd "x" (Just pr) $ Lmbd "y" (Just (Atom "A")) $ Appl (Var "snd") (Var "x")
kb1 = Map.fromList [(f1, p1)]

f2 = Impl (Atom "A") (Atom "A")

main = do
    printProof kb1 f1
    printProofE f2
    printProofE $ Impl (Atom "A") (Disj (Atom "A") (Atom "B"))
    printProofE $ Impl (Atom "A") (Impl (Atom "B") (Conj (Atom "A") (Atom "B")))
