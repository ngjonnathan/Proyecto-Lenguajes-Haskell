module Definitions (Term(..), Equation(..), Sust(..), (\/), (/\), (==>), (<==>), (!<==>), (===), (=:) )where

data Term = T | F | Var Char | Or Term Term | And Term Term | Imp Term Term | DoubleImp Term Term | DoubleNotImp Term Term | Not Term deriving (Eq)
data Equation = Eq Term Term deriving (Eq)
data Sust = Sust Term Term

--Operators
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

-- Precedence
infixl 8 \/
infixl 8 /\
infixr 7 ==>
infixl 6 <==>
infixl 6 !<==> 
infixl 5 === 