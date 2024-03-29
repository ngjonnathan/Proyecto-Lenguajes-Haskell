{-# LANGUAGE FlexibleInstances #-}
module Functions where
import Term
import Theorems
import ShowTerm

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
sust _ _ = error "substitute variable for expression."

instantiate :: Equation -> Sust -> Equation
instantiate (Equation t1 t2) su = Equation (sust su t1) (sust su t2)

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equation e1 e2) termE (Var z) = Equation t1 t2
                                         where t1 = sust (Sust1 (Var z) e1) termE
                                               t2 = sust (Sust1 (Var z) e2) termE

infer :: Float -> Sust -> Term -> Term -> Equation
infer n sus z termE = leibniz (instantiate (prop n) sus) termE z

step :: Float -> Sust -> Term -> Term -> Term -> Term
step n sus z termE term = stepAux $ infer n sus z termE
    where stepAux (Equation e1 e2)
                    | e1 == term = e2
                    | e2 == term = e1
                    | otherwise = error "invalid inference rule."


class Statement tuple where
    statement :: Float -> () -> tuple -> () -> () -> Term -> Term -> Term -> IO Term

instance Statement Sust where
    statement n _ (Sust1 x1 t1) _ _ z termE termAux =  
        do
            let sus= Sust1 x1 t1
            let term = step n sus z termE termAux
            putStrLn $ id "===<statement " ++ show n ++ " with " ++ showSust(sus) ++ " using lambda " ++ showTerm(z) ++ " (" ++ showTerm(termE) ++ ")>"
            print term
            return term


instance Statement (Term,Sust,Term) where
    statement n _ (t1, Sust1 x1 t2, x2) _ _ z termE termAux =   
        do
            let sus= Sust2 x1 x2 t1 t2
            let term = step n sus z termE termAux
            putStrLn $ id "===<statement " ++ show n ++ " with " ++ showSust(sus) ++ " using lambda " ++ showTerm(z) ++ " (" ++ showTerm(termE) ++ ")>"
            print term
            return term   

instance Statement (Term,Term,Sust,Term,Term) where
    statement n _ (t1, t2, Sust1 x1 t3, x2, x3) _ _ z termE termAux =   
        do
            let sus= Sust3 x1 x2 x3 t1 t2 t3
            let term = step n sus z termE termAux 
            putStrLn $ id "===<statement " ++ show n ++ " with " ++ showSust(sus) ++ " using lambda " ++ showTerm(z) ++ " (" ++ showTerm(termE) ++ ")>"
            print term
            return term   

proof :: Equation -> IO Term
proof (Equation t1 t2) = do 
                    putStrLn $ id "prooving " ++ showTerm(t1) ++ " === " ++ showTerm(t2) ++ "\n"
                    putStrLn $ id showTerm(t1)
                    return(t1)

done :: Equation -> Term -> IO ()
done (Equation t1 t2) t 
    | t == t2 = putStrLn "\nproof successful"
    | otherwise = error "proof unsuccessful"












