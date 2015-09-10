-- \x -> \y -> x >> y :: Monad m => m a -> m b -> m b

--
m n = if n > 100
        then n - 10
        else m(n + 11)

-- m(17) = 95
-- m(35) = 91
-- m(88) = 100

-- 
pre n = n - 1
suc n = n + 1
add x y = if y == 0
             then x
             else add (suc x) (pre y)
mult x y = if y == 1
             then x
	         else add x (mult x (pre y))