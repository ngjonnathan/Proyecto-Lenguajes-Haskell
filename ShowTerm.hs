module ShowTerm where
import Definitions

showTerm :: Term -> String
showTerm (Var i) = show i
showTerm (T) = "true"
showTerm (F) = "false"

showTerm (Or (Var i) (Var j)) = showTerm(Var i) ++ "\\/" ++ showTerm(Var j)
showTerm (Or (Var i) t) = showTerm(Var i) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm (Or t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var i)
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"

showTerm (And (Var i) (Var j)) = showTerm(Var i) ++ "/\\" ++ showTerm(Var j)
showTerm (And (Var i) t) = showTerm(Var i) ++ " /\\ (" ++ showTerm(t) ++ ")"
showTerm (And t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " /\\ " ++ showTerm(Var i)
showTerm (And t1 t2) = "(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"

showTerm (Imp (Var i) (Var j)) = showTerm(Var i) ++ "==>" ++ showTerm(Var j)
showTerm (Imp (Var i) t) = showTerm(Var i) ++ " ==> (" ++ showTerm(t) ++ ")"
showTerm (Imp t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " ==> " ++ showTerm(Var i)
showTerm (Imp t1 t2) = "(" ++ showTerm t1 ++ ") ==> (" ++ showTerm t2 ++ ")"

showTerm (DoubleImp (Var i) (Var j)) = showTerm(Var i) ++ "<==>" ++ showTerm(Var j)
showTerm (DoubleImp (Var i) t) = showTerm(Var i) ++ " <==> (" ++ showTerm(t) ++ ")"
showTerm (DoubleImp t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " <==> " ++ showTerm(Var i)
showTerm (DoubleImp t1 t2) = "(" ++ showTerm t1 ++ ") <==> (" ++ showTerm t2 ++ ")"

showTerm (DoubleNotImp (Var i) (Var j)) = showTerm(Var i) ++ "!<==>" ++ showTerm(Var j)
showTerm (DoubleNotImp (Var i) t) = showTerm(Var i) ++ " !<==> (" ++ showTerm(t) ++ ")"
showTerm (DoubleNotImp t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " !<==> " ++ showTerm(Var i)
showTerm (DoubleNotImp t1 t2) = "(" ++ showTerm t1 ++ ") !<==> (" ++ showTerm t2 ++ ")"

showTerm (Not (Var i)) = "neg" ++ showTerm(Var i)
showTerm (Not t) = "neg(" ++ showTerm t ++ ")"

instance Show Term where show = showTerm

showEq :: Equation -> String
showEq (Equation t1 t2) = showTerm t1 ++ " === " ++ showTerm t2

instance Show Equation where show = showEq

showSust :: Sust -> String
showSust (Sust1 (Var i) t) = "[" ++ show i ++ ":=" ++ showTerm t ++ "]"
showSust (Sust2 (Var i) (Var j) t1 t2) = "[" ++ show i ++ ", " ++ show j ++ ":=" ++ showTerm t1 ++ ", " ++ showTerm t2 ++ "]"

instance Show Sust where show = showSust