import Data.Maybe

hailstone :: Int -> Int
hailstone n
 | even n     = div n 2
 | otherwise  = 3*n + 1

hailSeq, hailSeq' :: Int -> [Int]
hailSeq 1 = [1]
hailSeq n = n : hailSeq (hailstone n)

hailSeq' n = (takeWhile (/=1) $ iterate hailstone n) ++ [1]

join :: [Char] -> [[Char]] -> [Char]
join _ [] = ""
join sep (w:ws) = foldl sepjoin w ws
  where sepjoin x y = x ++ sep ++ y

-- a more-awkward foldr version
join' :: [Char] -> [[Char]] -> [Char]
join' _ [] = ""
join' sep ws = foldr sepjoin (last ws) (init ws)
  where sepjoin x y = x ++ sep ++ y

merge :: (Ord t) => [t] -> [t] -> [t]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x<y        = x : merge xs (y:ys)
  | otherwise  = y : merge (x:xs) ys

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge pt1s pt2s
  where pt1s = mergesort pt1
        pt2s = mergesort pt2
        (pt1,pt2) = splitAt halfway xs
        halfway = (length xs) `div` 2

findElt, findElt', findElt'' :: (Eq a) => a -> [a] -> Maybe Int
findElt _ [] = Nothing
findElt x (y:ys)
   | x==y        = Just 0
   | otherwise   = maybe Nothing (\x -> Just (x+1)) findys
   where findys = findElt x ys

findElt' _ [] = Nothing
findElt' x (y:ys)
   | x==y       = Just 0
   | otherwise  = case findys of
                  Just a -> Just (a+1)
                  Nothing -> Nothing
   where findys = findElt x ys

findElt'' x ys = listToMaybe allPos
  where allPos = map fst $ filter isX (zip [0..] ys)
        isX (pos, val) = val==x 



