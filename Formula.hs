-- 
-- Formula type and utilities.
--

module Formula where

data Formula =
    Atom String |
    Conj Formula Formula |
    Disj Formula Formula |
    Impl Formula Formula |
    PrnF Formula
  deriving (Eq, Ord)

instance Show Formula where
    show (Atom a) = a
    show (Conj a b) = show a ++ " ^ " ++ show b
    show (Disj a b) = show a ++ " v " ++ show b
    show (Impl a b) = show a ++ " => " ++ show b
    show (PrnF a) = "(" ++ show a ++ ")"
