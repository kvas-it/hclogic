--
-- Proof type and utilities.
--

module Proof where

import Formula

data Proof =
    Var String |
    Appl Proof Proof |
    Lmbd String (Maybe Formula) Proof |
    Pair Proof Proof |
    PrnP Proof
  deriving (Eq, Ord)

instance Show Proof where
    show (Var a) = a
    show (Appl a b) = show a ++ " " ++ show b
    show (Lmbd x (Just f) p) = "(\\" ++ x ++ " : " ++ show f ++ "). " ++ show p
    show (Lmbd x Nothing p) = "\\" ++ x ++ ". " ++ show p
    show (Pair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
    show (PrnP p) = "(" ++ show p ++ ")"

-- Shortcuts for builtin function application.
appl funcName p = Appl (Var funcName) p
appl2 funcName p q = Appl (appl funcName p) q
appl3 funcName p q r = Appl (appl2 funcName p q) r 

varsOf :: Proof -> [String]
varsOf (Var a) = [a]
varsOf (Appl a b) = varsOf a ++ varsOf b
varsOf (Lmbd a _ b) = [a] ++ varsOf b
varsOf (PrnP a) = varsOf a

-- Parenthesize the proof to make it unambiguous.
parenP :: Proof -> Proof
parenP (Appl a b@(Appl _ _)) = Appl (parenP a) $ PrnP (parenP b)
parenP (Appl a b) = Appl (parenP a) (parenP b)
parenP (Lmbd x Nothing p) = Lmbd x Nothing (parenP p)
parenP (Lmbd x (Just f) p) = Lmbd x (Just $ parenF f) (parenP p)
parenP (Pair a b) = Pair (parenP a) (parenP b)
parenP p = p
