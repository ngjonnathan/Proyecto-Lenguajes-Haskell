data Term = T | F | Var Char | Or Term Term | And Term Term | Imp Term Term | DoubleImp Term Term | DoubleNotImp Term Term | Not Term deriving (Eq)
data Equation = Eq Term Term deriving (Eq)
data Sust = Sust Term Term

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

(===) :: Term -> Term -> Equation
(===) t1 t2 = Eq t1 t2

(=:) :: Term -> Term -> Sust
(=:) t2 t1 = Sust t1 t2

-- Precedencia operadores--
infixl 8 \/
infixl 8 /\
infixr 7 ==>
infixl 6 <==>
infixl 6 !<==> 
infixl 5 === 

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

showEq :: Equation -> String
showEq (Eq t1 t2) = showTerm t1 ++ " === " ++ showTerm t2

instance Show Equation where show = showEq

--- Funciones----
sust :: Term -> Term -> Term -> Term
sust (Var x) term (Var z) = if (x == z) then term else Var z
sust (Var x) term (Or t1 t2) = Or (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (And t1 t2) = And (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (Imp t1 t2) = Imp (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (DoubleImp t1 t2) = DoubleImp (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (DoubleNotImp t1 t2) = DoubleNotImp (sust (Var x) term t1) (sust (Var x) term t2)
sust (Var x) term (Not t1) = Not (sust (Var x) term t1)
sust _ _ _ = error "Debe sustituir una variable por una expresion"
