module Functions where
import Definitions

sust :: Sust -> Term -> Term
sust _ T = T
sust _ F = F
sust (Sust1 (Var x) term) (Var z) = if (x == z) then term else Var z
sust (Sust2 (Var x1) (Var x2) t1 t2) (Var z)
        |  x1 == z  = t1
        |  x2 == z  = t2
        | otherwise = Var z
sust (Sust3 (Var x1) (Var x2) (Var x3) t1 t2 t3) (Var z)
        |  x1 == z  = t1
        |  x2 == z  = t2
        |  x3 == z  = t3
        | otherwise = Var z   
sust su (Or e1 e2) = Or (sust su e1) (sust su e2)
sust su (And e1 e2) = And (sust su e1) (sust su e2)
sust su (Imp e1 e2) = Imp (sust su e1) (sust su e2)
sust su (DoubleImp e1 e2) = DoubleImp (sust su e1) (sust su e2)
sust su (DoubleNotImp e1 e2) = DoubleNotImp (sust su e1) (sust su e2)
sust su (Not e1) = Not (sust su e1)
sust _ _ = error "Debe sustituir una variable por una expresion"

instantiate :: Equation -> Sust -> Equation
instantiate (Equation t1 t2) su = Equation (sust su t1) (sust su t2)

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equation e1 e2) termE (Var z) = Equation t1 t2
                                         where t1 = sust (Sust1 (Var z) e1) termE
                                               t2 = sust (Sust1 (Var z) e2) termE