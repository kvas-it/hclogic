-- 
-- Formula type and utilities.
--

module Formula (Formula (Atom, Conj, Disj, Impl), parenF) where

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

-- Parenthesize the formula to make it unambiguous.
parenF :: Formula -> Formula
parenF (PrnF f) = PrnF $ parenF f
parenF (Conj a b) = Conj (pC $ parenF a) (pC $ parenF b)
parenF (Disj a b) = Disj (pD $ parenF a) (pD $ parenF b)
parenF (Impl a b) = Impl (pIL $ parenF a) (parenF b)
parenF f = f

pC f@(Disj a b) = PrnF f
pC f@(Impl a b) = PrnF f
pC f = f

pD f@(Impl a b) = PrnF f
pD f = f

pIL f@(Impl a b) = PrnF f
pIL f = f
