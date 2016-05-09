module Functions(sust) where
import Definitions

sust :: Sust -> Term -> Term
sust (Sust1 (Var x) term) (Var z) = if (x == z) then term else Var z
sust (Sust1 (Var x) term) (Or t1 t2) = Or (sust (Sust1 (Var x) term) t1) (sust (Sust1 (Var x) term) t2)
sust (Sust1 (Var x) term) (And t1 t2) = And (sust (Sust1 (Var x) term) t1) (sust (Sust1 (Var x) term) t2)
sust (Sust1 (Var x) term) (Imp t1 t2) = Imp (sust (Sust1 (Var x) term) t1) (sust (Sust1 (Var x) term) t2)
sust (Sust1 (Var x) term) (DoubleImp t1 t2) = DoubleImp (sust (Sust1 (Var x) term) t1) (sust (Sust1 (Var x) term) t2)
sust (Sust1 (Var x) term) (DoubleNotImp t1 t2) = DoubleNotImp (sust (Sust1 (Var x) term) t1) (sust (Sust1 (Var x) term) t2)
sust (Sust1 (Var x) term) (Not t1) = Not (sust (Sust1 (Var x) term) t1)
sust _ _ = error "Debe sustituir una variable por una expresion"
