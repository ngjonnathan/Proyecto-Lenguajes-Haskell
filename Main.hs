module Main where
import Definitions 
import Vars
import ShowTerm
import Functions


proof :: Equation -> IO Term
proof (Equation t1 t2) = do 
					putStrLn $ id "prooving " ++ showTerm(t1) ++ " === " ++ showTerm(t2) ++ "\n"
					putStrLn $ id showTerm(t1)
					return(t1)

done :: Equation -> Term -> IO ()
done (Equation t1 t2) t 
	| t == t2 = putStrLn "\nproof successful"
	| otherwise = error "proof unsuccessful"


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












