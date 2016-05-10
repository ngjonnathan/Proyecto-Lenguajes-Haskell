module Functions where
import Definitions

sust :: Sust -> Term -> Term
sust _ T = T
sust _ F = F
sust (Sust1 (Var x) term) (Var z) = if (x == z) then term else Var z
sust (Sust1 (Var x) term) (Or e1 e2) = Or (sust (Sust1 (Var x) term) e1) (sust (Sust1 (Var x) term) e2)
sust (Sust1 (Var x) term) (And e1 e2) = And (sust (Sust1 (Var x) term) e1) (sust (Sust1 (Var x) term) e2)
sust (Sust1 (Var x) term) (Imp e1 e2) = Imp (sust (Sust1 (Var x) term) e1) (sust (Sust1 (Var x) term) e2)
sust (Sust1 (Var x) term) (DoubleImp e1 e2) = DoubleImp (sust (Sust1 (Var x) term) e1) (sust (Sust1 (Var x) term) e2)
sust (Sust1 (Var x) term) (DoubleNotImp e1 e2) = DoubleNotImp (sust (Sust1 (Var x) term) e1) (sust (Sust1 (Var x) term) e2)
sust (Sust1 (Var x) term) (Not e1) = Not (sust (Sust1 (Var x) term) e1)
sust _ _ = error "Debe sustituir una variable por una expresion"

sust2 :: (Term,Sust,Term) -> Term -> Term
sust2 _ T = T
sust2 _ F = F
sust2 (t2, (Sust1 (Var x1) t1), Var x2) (Var z)
        |  x1 == z  = t1
        |  x2 == z  = t2
        | otherwise = Var z
sust2 (t2, (Sust1 (Var x1) t1), Var x2) (Or e1 e2) = Or (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e1) (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e2)
sust2 (t2, (Sust1 (Var x1) t1), Var x2) (And e1 e2) = And (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e1) (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e2)
sust2 (t2, (Sust1 (Var x1) t1), Var x2) (Imp e1 e2) = Imp (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e1) (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e2)
sust2 (t2, (Sust1 (Var x1) t1), Var x2) (DoubleImp e1 e2) = DoubleImp (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e1) (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e2)
sust2 (t2, (Sust1 (Var x1) t1), Var x2) (DoubleNotImp e1 e2) = DoubleNotImp (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e1) (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e2)
sust2 (t2, (Sust1 (Var x1) t1), Var x2) (Not e1) = Not (sust2 (t2, (Sust1 (Var x1) t1), Var x2) e1)
sust2 _ _ = error "Debe sustituir una variable por una expresion"


sust3 :: (Term,Term,Sust,Term,Term) -> Term -> Term
sust3 _ T = T
sust3 _ F = F
sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) (Var z)
        |  x1 == z  = t1
        |  x2 == z  = t2
        |  x3 == z  = t3
        | otherwise = Var z
sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) (Or e1 e2) = Or (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e1) (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e2)
sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) (And e1 e2) = And (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e1) (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e2)
sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) (Imp e1 e2) = Imp (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e1) (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e2)
sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) (DoubleImp e1 e2) = DoubleImp (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e1) (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e2)
sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) (DoubleNotImp e1 e2) = DoubleNotImp (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e1) (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e2)
sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) (Not e1) = Not (sust3 (t3, t2, (Sust1 (Var x1) t1), Var x2, Var x3) e1)
sust3 _ _ = error "Debe sustituir una variable por una expresion"