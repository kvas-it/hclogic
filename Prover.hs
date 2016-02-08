-- 
-- Generate constructive proofs for formulas.
--

import qualified Data.Map as Map

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

data Proof =
    Var String |
    Appl Proof Proof |
    Lmbd String (Maybe Formula) Proof |
    PrnP Proof
  deriving (Eq, Ord)

instance Show Proof where
    show (Var a) = a
    show (Appl a b) = show a ++ " " ++ show b
    show (Lmbd x (Just f) p) = "(\\" ++ x ++ " : " ++ show f ++ "). " ++ show p
    show (Lmbd x Nothing p) = "\\" ++ x ++ ". " ++ show p
    show (PrnP p) = "(" ++ show p ++ ")"

-- Knowledge base.
type KB = Map.Map Formula Proof

prove :: KB -> Formula -> Maybe Proof
prove kb f
    | Map.member f kb  = Just $ kb Map.! f
    | otherwise        = Nothing

premise = Conj (Atom "A") (Atom "B")
conclusion = Impl (Atom "A") (Atom "B")
f1 = Impl (PrnF premise) conclusion
p1 = Lmbd "x" (Just premise) $ Lmbd "y" (Just (Atom "A")) $ Appl (Var "snd") (Var "x")

kb1 = Map.fromList [(f1, p1)]

main = do
    putStrLn $ show f1
    putStrLn $ show p1
    putStrLn $ show $ prove kb1 f1
