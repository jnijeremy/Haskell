-- #1
hailstone x
	| even x = div x 2
	| otherwise = x*3+1

-- #2
hailLen x
	| x == 1 = 0
	| otherwise = 1 + hailLen (hailstone x)

-- #3
divisors :: Int -> [Int]
divisors n = [ i | i <- [ 2..( n `div` 2 ) ], n `mod` i == 0]

primes :: Int -> [Int]
primes n = [ i | i <- [2..n], divisors i == [] ]

-- #4
join _ [] = []
join _ [x] = x
join separator (x:xs) = x ++ separator ++ (join separator xs)

-- #5
pythagorean :: Int -> [(Int, Int, Int)]
pythagorean x = [ (a,b,c) | a <- [1..x], b<- [1..x], c<-[1..x], a^2+b^2 == c^2, a<=b, b<=c, c<=x]
