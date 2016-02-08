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

printProof f = do
    putStrLn ""
    putStrLn ("proving " ++ show f)
    printPr $ prove emptyKB f
  where
    printPr Nothing = putStrLn "failed"
    printPr (Just p) = putStrLn $ ">>> " ++ show p

main = do
    printProof $ Impl (Atom "A") (Impl (Atom "B") (Conj (Atom "A") (Atom "B")))
    printProof $ Impl (Atom "A") (Disj (Atom "A") (Atom "B"))
    printProof $ Impl (Atom "A") (Impl (Atom "B") (Conj (Atom "A") (Atom "B")))
    printProof $ Impl (Conj (Atom "A") (Atom "B")) (Disj (Atom "A") (Atom "B"))
