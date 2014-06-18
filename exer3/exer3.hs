import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate


-- #1
merge :: (Ord a) => [ a] -> [ a] -> [ a]
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys)
	| x < y = x:(merge xs (y:ys))
	| otherwise = y:(merge (x:xs) ys)

-- #2
hailstone :: Int -> Int
hailstone x
	| even x = div x 2
	| otherwise = x*3+1

hailLen :: Int -> Int
hailLen n = hailTail 0 n
  where
    hailTail a 1 = a
    hailTail a n = hailTail (a+1) (hailstone n)

-- #3
fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

-- #4
fact' :: Int -> Int
fact' 0 = 1
fact' n = foldl (*) 1 [1..n] 

-- #5
-- fromGregorian :: Integer -> Int -> Int -> Day
daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
  where jan1 = fromGregorian y 1 1
        dec31 = fromGregorian y 12 31

-- #6 
--mondayStartWeek :: Day -> (Int, Int) Get the number of the Monday
--snd :: (a, b) -> b
isFriday :: Day -> Bool
isFriday day 
	| (snd (mondayStartWeek day)) == 5 = True
	| otherwise = False

-- #7
divisors :: Int -> [Int]
divisors n = [ i | i <- [ 2..( n `div` 2 ) ], n `mod` i == 0]

isPrime :: Int -> Bool
isPrime n 
	| divisors n == [] = True
	| otherwise = False

getDay (_,_,z) = z

isPrimeDay :: Day -> Bool
isPrimeDay day
	| isPrime (getDay (toGregorian day)) = True
	| otherwise = False

-- #8
primeFridays :: Integer -> [Day]
primeFridays year = filter isPrimeDay (filter isFriday (daysInYear year))







