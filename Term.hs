module Term where

data Term = Constant String | Var Char | Or Term Term | And Term Term | Imp Term Term | DoubleImp Term Term | DoubleNotImp Term Term | Not Term deriving (Eq)

data Equation = Equation Term Term deriving (Eq)

data Sust = Sust1 Term Term | Sust2 Term Term Term Term | Sust3 Term Term Term Term Term Term

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
(===) t1 t2 = Equation t1 t2

(=:) :: Term -> Term -> Sust
(=:) t1 (Var i) = Sust1 (Var i) t1
(=:) _ _ = error "Error: substitute variable for expression"

-- Precedence

infixl 8 \/
infixl 8 /\
infixr 7 ==>
infixl 6 <==>
infixl 6 !<==> 
infixl 5 ===
infixl 5 =: 

with :: ()
with = ()

using :: ()
using = ()

lambda :: ()
lambda = ()

true :: Term
true = Constant "true"

false :: Term
false = Constant "false"

a :: Term
a = Var 'a'

b :: Term
b = Var 'b'

c :: Term
c = Var 'c'

d :: Term
d = Var 'd'

e :: Term
e = Var 'e'

f :: Term
f = Var 'f'

g :: Term
g = Var 'g'

h :: Term
h = Var 'h'

i :: Term
i = Var 'i'

j :: Term
j = Var 'j'

k :: Term
k = Var 'k'

l :: Term
l = Var 'l'

m :: Term
m = Var 'm'

n :: Term
n = Var 'n'

o :: Term
o = Var 'o'

p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

s :: Term
s = Var 's'

t :: Term
t = Var 't'

u :: Term
u = Var 'u'

v :: Term
v = Var 'v'

w :: Term
w = Var 'w'

x :: Term
x = Var 'x'

y :: Term
y = Var 'y'

z :: Term
z = Var 'z'
