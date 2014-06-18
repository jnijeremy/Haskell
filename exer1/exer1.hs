det a b c = b^2 - 4*a*c
quadsol1 a b c = (-b - sqrt(det a b c))/2*a
quadsol2 a b c = (-b + sqrt(det a b c))/2*a

square x = x^2

third xs = xs !! 2

--iseven x = (mod x 2) 
--hailstone x =  (if (iseven x)==0 then (div x 2) else x * 3 + 1)

hailstone x
	| even x = div x 2
	| otherwise = x*3+1