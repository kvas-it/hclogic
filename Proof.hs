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

-- Shortcut for a builtin function application.
appl funcName p = Appl (Var funcName) p

varsOf :: Proof -> [String]
varsOf (Var a) = [a]
varsOf (Appl a b) = varsOf a ++ varsOf b
varsOf (Lmbd a _ b) = [a] ++ varsOf b
varsOf (PrnP a) = varsOf a
