-- 
-- Generate constructive proofs for formulas.
--

data Formula =
    Atom String |
    Conj Formula Formula |
    Disj Formula Formula |
    Impl Formula Formula |
    Prns Formula

instance Show Formula where
    show (Atom a) = a
    show (Conj a b) = show a ++ " ^ " ++ show b
    show (Disj a b) = show a ++ " v " ++ show b
    show (Impl a b) = show a ++ " => " ++ show b
    show (Prns a) = "(" ++ show a ++ ")"

main =
    putStrLn $ show $ Impl (Prns (Conj (Atom "A") (Atom "B"))) (Atom "C")
