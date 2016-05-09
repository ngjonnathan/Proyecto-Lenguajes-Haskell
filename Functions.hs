module Functions(sust) where
import Definitions

sust :: Term -> Term -> Term -> Term
sust (Var x) term (Var z) = if (x == z) then term else Var z
sust (Var x) term (Or t1 t2) = Or (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (And t1 t2) = And (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (Imp t1 t2) = Imp (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (DoubleImp t1 t2) = DoubleImp (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (DoubleNotImp t1 t2) = DoubleNotImp (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (Not t1) = Not (sust (Var x) term t1)
sust _ _ _ = error "Debe sustituir una variable por una expresion"