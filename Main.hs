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
    putStrLn ("proving " ++ show (parenF f))
    printPr $ prove emptyKB f
  where
    printPr Nothing = putStrLn "failed"
    printPr (Just p) = putStrLn $ ">>> " ++ show (parenP p)

proveImpl a b = printProof $ Impl a b

main = do
    proveImpl (Atom "A") (Disj (Atom "A") (Atom "B"))
    proveImpl (Atom "A") (Impl (Atom "B") (Conj (Atom "A") (Atom "B")))
    proveImpl (Conj (Atom "A") (Atom "B")) (Disj (Atom "A") (Atom "B"))
    proveImpl (Conj (Impl (Atom "A") (Atom "B")) (Impl (Atom "B") (Atom "C")))
              (Impl (Atom "A") (Atom "C"))
