module Vars where
import Definitions (Term(Var,F,T))

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

t :: Term
t = Var 't'

a :: Term
a = Var 'a'

b :: Term
b = Var 'b'

c :: Term
c = Var 'c'

z :: Term
z = Var 'z'


-- Continuar please