import Data.Maybe

-- #2
hailstone :: Int -> Int
hailstone x
	| even x = div x 2
	| otherwise = x*3+1

hailSeq :: Int -> [Int]
hailSeq 1 = [1]
hailSeq x = x : (hailSeq x')
	where x' = hailstone x

-- #3
-- iterate f x == [x, f x, f (f x), ...]
hailSeq' :: Int -> [Int]
hailSeq' x = takeWhile (/=1) (iterate hailstone x) ++ [1]

-- #4
join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join seperator xs = foldl add [] xs
	where 
		add [] b = b
		add a b = a ++ seperator ++ b

-- #5
merge :: (Ord a) => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys)
	| x < y = x:(merge xs (y:ys))
	| otherwise = y:(merge (x:xs) ys)

half :: [a] -> ([a],[a])
half [] = ([],[])
half list = splitAt (div (length list) 2) list

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort list = merge (mergesort left) (mergesort right)
	where  (left,right) = half list

-- #6
find :: (Ord a, Num b) => a -> [a] -> b -> Maybe b
find _ [] _ = Nothing
find a (x:xs) count
	| a == x = listToMaybe [count]
	| otherwise = find a xs (count+1)

findElt :: (Ord a, Num b) => a -> [a] -> Maybe b
findElt a [] = Nothing
findElt a (x:xs) = find a (x:xs) 0




































