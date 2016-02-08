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

_A = Atom "A"
_B = Atom "B"
_C = Atom "C"

main = do
    proveImpl _A (_A `Disj` _B)
    proveImpl _A (_B `Impl` (_A `Conj` _B))
    proveImpl (_A `Conj` _B) (_A `Disj` _B)
    proveImpl ((_A `Impl` _B) `Conj` (_B `Impl` _C)) (_A `Impl` _C)
    proveImpl (_A `Conj` (_B `Conj` _C)) _C
    proveImpl ((_A `Disj` _B) `Conj` ((_A `Impl` _C) `Conj` (_B `Impl` _C))) _C
    proveImpl ((_A `Impl` _C) `Conj` (_B `Impl` _C)) ((_A `Disj` _B) `Impl` _C)
