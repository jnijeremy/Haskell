myAnd False _ = False
myAnd True a = a

elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = elem' a xs   

addLists xs ys = map ( uncurry (+) ) ( zip xs ys )

myPowerStrict m _ 0 = m
myPowerStrict m x y = (myPowerStrict $! m*x) x (y-1)


myPower m _ 0 = m
myPower m x y = myPower (m*x) x (y-1)