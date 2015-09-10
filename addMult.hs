-- Addition and Multiplication in Haskell
-- Use of only pre and suc functions, == comparison, recursion and if-then-else statements

-- Test
-- \x -> \y -> x >> y :: Monad m => m a -> m b -> m b

-- Random method
m n = if n > 100
        then n - 10
        else m(n + 11)

-- m(17) = 95
-- m(35) = 91
-- m(88) = 100

-- Helper functions
pre n = n - 1
suc n = n + 1

--Add function
add x y = if y == 0
             then x
             else add (suc x) (pre y)
--Mult function , uses the Add function
mult x y = if y == 1
             then x
	         else add x (mult x (pre y))
