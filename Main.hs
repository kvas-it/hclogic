--
-- Main module that proves some examples and prints them out.
--

import qualified Data.Map as Map
import Control.Monad

import Formula
import Proof
import Prover

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
