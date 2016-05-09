data Term = T | F | Var Char | Or Term Term | And Term Term | Imp Term Term | DoubleImp Term Term | DoubleNotImp Term Term | Not Term
data Equation = Term Term
data Sust

--------------------------------- Operadores ----------------------------------
(\/) :: Term -> Term -> Term
(\/) t1 t2 = Or t1 t2

(/\) :: Term -> Term -> Term
(/\) t1 t2 = And t1 t2

(==>) :: Term -> Term -> Term
(==>) t1 t2 = Imp t1 t2

(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = DoubleImp t1 t2

(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = DoubleNotImp t1 t2

neg :: Term -> Term
neg t1 = Not t1

--(===) :: Term -> Term -> Equation
--(===) t1 t2 = Equation t1 t2

-- Precedencia operadores--
infixl 3 \/
infixl 3 /\
infixr 2 ==>
infixl 1 <==>
infixl 1 !<==> 

true :: Term
true = T

false :: Term
false = F

p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

---------------------------------- Show ---------------------------------------
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