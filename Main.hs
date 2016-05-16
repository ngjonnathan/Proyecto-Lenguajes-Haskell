module Main where
import Definitions 
import Vars
import ShowTerm
import Functions

term :: Equation
term = (p <==> q) <==> r === p <==> (q <==> r)

sust1 :: Sust
sust1 = Sust1 p (q<==>p\/r)

sust2 :: Sust
sust2 = Sust2 p q (r\/p) (q==>a)


statement :: Term -> IO Term 
statement term = return term

main = statement (a/\true) 


--main = do
--        let prueba = statement 3.1 with (p <==> q,p =: p, r) using lambda z (z)
--        print $ prueba ((p <==> q) <==> (q <==> p))

--verify= let theorem = (true === ((p <==> p) <==> (q <==> q))) in
--		proof theorem
--		>>=
--		statement 3.2
--		>>=
--		statement 3.1
--		>>=
--		statement 3.3
--		>>=
--		statement 3.3
--		>>=
--		statement 3.1
--		>>=
--		statement 3.1
--		>>=
--		statement 3.1
--		>>=
--		done theorem



