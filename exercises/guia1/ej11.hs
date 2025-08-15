data Polinomio a = X
            | Cte a
            | Suma (Polinomio a) (Polinomio a)
            | Prod (Polinomio a) (Polinomio a)

evaluar :: Num a => a -> Polinomio a -> a
evaluar x X = x
evaluar _ (Cte c) = c
evaluar x (Suma p q) = evaluar x p + evaluar x q
evaluar x (Prod p q) = evaluar x p * evaluar x q

foldPol :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPol cX cCte cSuma cProd  p = case p of
        X -> cX
        Cte p -> cCte p
        Suma p q -> cSuma (acc p) (acc q)
        Prod p q -> cProd (acc p) (acc q)
        where acc = foldPol cX cCte cSuma cProd

evaluar2 v = foldPol v (\x -> x) (+) (*)