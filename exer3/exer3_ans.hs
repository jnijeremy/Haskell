import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

merge :: (Ord t) => [t] -> [t] -> [t]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x<=y        = x : merge xs (y:ys)
  | otherwise   = y : merge (x:xs) ys

hailstone :: Integer -> Integer
hailstone n
 | even n     = div n 2
 | otherwise  = 3*n + 1

hailLen :: Integer -> Int
hailLen n = hailTail 0 n
  where
    hailTail a 1 = a
    hailTail a n = hailTail (a+1) (hailstone n)

fact 0 = 1
fact n = n * fact (n-1)

fact' n = foldl (*) 1 [1..n]

daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
  where jan1 = fromGregorian y 1 1
        dec31 = fromGregorian y 12 31

isFriday :: Day -> Bool
isFriday day = (snd (mondayStartWeek day)) == 5

divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

isPrimeDay :: Day -> Bool
isPrimeDay day = (divisors (getDay' day)) == []
  where getDay (y,m,d) = d
        getDay' = getDay.toGregorian

primeFridays :: Integer -> [Day]
primeFridays y = [day | day <- daysInYear y, isFriday day, isPrimeDay day]

