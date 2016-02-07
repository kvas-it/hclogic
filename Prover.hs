-- 
-- Generate constructive proofs for formulas.
--

data Formula =
    Atom String |
    Conj Formula Formula |
    Disj Formula Formula |
    Impl Formula Formula |
    PrnF Formula

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

instance Show Proof where
    show (Var a) = a
    show (Appl a b) = show a ++ show b
    show (Lmbd x (Just f) p) = "(\\" ++ x ++ " : " ++ show f ++ "). " ++ show p
    show (Lmbd x Nothing p) = "\\" ++ x ++ ". " ++ show p
    show (PrnP p) = "(" ++ show p ++ ")"

main = do
    putStrLn $ show $ Impl (PrnF (Conj (Atom "A") (Atom "B"))) (Atom "C")
    putStrLn $ show $ Lmbd "x" (Just (Conj (Atom "A") (Atom "B")))
        (PrnP (Appl (Var "f") (Var "x")))
    putStrLn $ show $ Lmbd "x" Nothing $ Lmbd "y" Nothing $
        Appl (Appl (Var "f") (Var "x")) (Var "y")
