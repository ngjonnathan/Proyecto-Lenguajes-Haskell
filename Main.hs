module Main where
import Definitions 
import Vars
import ShowTerm
import Functions

--term :: Equation
--term = (p <==> q) <==> r === p <==> (q <==> r)

--sust1 :: Sust
--sust1 = Sust1 p (q<==>p\/r)

--sust2 :: Sust
--sust2 = Sust2 p q (r\/p) (q==>a)


--statement2 :: Term -> IO Term 
--statement2 term = return term

--main = do statement2 (a/\true) 

proof :: Equation-> IO Term
proof (Equation t1 _) = return t1

done :: Equation-> Term -> IO()
done (Equation _ t2) t1 = putStrLn $ if (t2==t1) then "proof successful" else "unsuccessful test" 

verify = let theorem =(true === ((p <==> p) <==> (q <==> q))) in 
		proof theorem
		>>=
		statement 3.2 with (p =: p) using lambda z (z)
		>>=
		statement 3.1 with (p <==> q,p =: p,r) using lambda z (z)
		>>=
		statement 3.3 with (p =: p) using lambda z (z <==> p)
		>>=
		statement 3.3 with (p =: p) using lambda z (p <==> z)
		>>=
		statement 3.1 with (p <==> q,q =: q,r) using lambda z (z)
		>>=
		statement 3.1 with (p,q =: q,r) using lambda z (z <==> q)
		>>=
		statement 3.1 with (p <==> p,q =: p,r) using lambda z (z)
		>>=
		done theorem


main = verify











