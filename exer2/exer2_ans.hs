hailstone n
 | even n     = div n 2
 | otherwise  = 3*n + 1     

hailLen 1 = 0
hailLen n = 1 + hailLen (hailstone n)   

divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

primes :: Int -> [Int]      
primes n = [i | i <- [2..n], divisors i == []] 

join _ [] = "" 
join _ (w:[]) = w
join sep (w:ws) = w ++ sep ++ join sep ws

pythagorean :: Int -> [(Int, Int, Int)]      
pythagorean n = [(a,b,c) | c<-[1..n], b<-[1..c-1], a<-[1..b-1], a*a+b*b==c*c]
     
      
       
        
        
  
  
  




