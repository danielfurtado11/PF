

{-

1.

a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
  
  -> [18,12,6]


b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]

  -> [6,12,18]


c) [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]

  -> [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]


d) [sum [y | y <- [1..x], odd y] | x <- [1..10]]

  -> [1,4,9,16,25]



2.

a) [1,2,4,8,16,32,64,128,256,512,1024]

  -> [2^x | x <- [1..10]]


b) [(1,5),(2,4),(3,3),(4,2),(5,1)]

  -> [(x,y) | x <- [1..5], y <- [1..5], x+y = 6]


c) [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]

  -> [[1..x] | x <- [1..5]]


d) [[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]]
  
  -> [take x [1,1,1,1,1] | x <- [1..5] ]


e) [1,2,6,24,120,720]
  
  -> [product [1..x]  | x <- [1..6]]

-}


-- 3 

import Data.Char
import Data.List

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])

digitAlpha (h:t)  | isDigit h = (h:p,q)
                  | isAlpha h = (p,h:q)
                  | otherwise = (p,q)

   where (p,q) = digitAlpha t



-- 4 

aux :: [Int] -> ([Int],[Int],[Int])

aux [] = ([],[],[])
aux (h:t) | h >  0 = (a,b,h:c)
          | h == 0 = (a,h:b,c)
          | h <  0 = (h:a,b,c)
  where (a,b,c) = aux t 


nzp :: [Int] -> (Int,Int,Int)
nzp (h:t) = (length x, length y, length z)
  where (x,y,z) = aux (h:t)


nzp' :: [Int] -> (Int,Int,Int)
nzp' [] = (0,0,0)
nzp' (h:t) | h >  0 = (a,b,1+c)
           | h == 0 = (a,1+b,c)
           | h <  0 = (1+a,b,c)
  where (a,b,c) = nzp' t



-- 5

myDivMod :: Integral a => a -> a -> (a, a)

myDivMod x y | x < y = (0,x)
              | otherwise = (1+p,m)
    where (p,m) = myDivMod (x-y) y 


-- 6

fromDigits :: [Int] -> Int

fromDigits l = faux 0 l

faux x [] = x
faux x (h:t) = faux (x + h * 10 ^ (length t)) t 



-- 7

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]

acc :: (Num a, Ord a) => [a] -> a -> a
acc [] m = m
acc (h:t) m = acc t (m+h)

maxSumInit' :: (Num a, Ord a) => [a] -> a
maxSumInit' l = acc l 0


-- 8 

--  fib :: Int -> Int
--  fib 0 = 0
--  fib 1 = 1
--  fib n = fib (n-1) + fib (n-2)

fib :: Int -> Int
fib x = fib' x (0,1)

fib' :: Int -> (Int,Int) -> Int
fib' 0 (a,b) = a
fib' 1 (a,b) = b
fib' x (a,b) = fib' (x-1) (b,b+a) 