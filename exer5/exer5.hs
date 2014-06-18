-- #1
pascal :: (Eq a, Num a, Num t) => a -> [t]
pascal 0 = [1]
pascal n = map (uncurry(+)) (zip (0:prev) (prev++[0]))
	where prev = pascal (n - 1)

-- #2
addPair :: (Integer, Integer) -> Integer
addPair = uncurry(+)

-- #3
withoutZeros :: (Eq a, Num a) => [a] -> [a]
withoutZeros = filter (/=0)

-- #4
fib :: (Eq a, Num a, Num a1) => a -> a1
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
fibs = map fib [0..]

-- #6
things :: [Integer]
things = 0 : 1 : zipWith (+) things (tail things)

