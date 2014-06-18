half x = x/2
isBig x = x>100
listify a b = [a, b]

myAnd False _ = False
myAnd True a = a

isZero 0 = True
isZero _ = False

listLength [] = 0
listLength (_:somelist) = 1 + (listLength somelist)

qsort [a] = [a]
qsort []=[]
qsort (x:xs) = smaller ++ [x] ++ larger
	where 
		smaller = qsort [a | a<-xs, a<=x]
		larger = qsort [a | a<-xs, a>x]

