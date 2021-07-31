-- Ficha 5

-- 1

-- a)

any' :: (a -> Bool) -> [a] -> Bool

any' test [] = False
any' test (h:t) | (test h)  = True
                | otherwise = False
                

-- b)


zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' test [] _ = []
zipWith' test _ [] = []
zipWith' test (x:xs) (y:ys) = (test x y) : zipWith' test xs ys


-- c) 

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' test [] = []
takeWhile' test (h:t) | (test h) = h: takeWhile' test t
                      | otherwise = []


-- d) 

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' test [] = []
dropWhile' test (h:t) | (test h) = dropWhile' test t
                      | otherwise = (h:t)


-- e)

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' test [] = ([],[])
span' test (h:t) | (test h) = ((h:p),(q))
                 | otherwise = ([],(h:t))
          where (p,q) = span' test t


-- f)

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' test x [] = []
deleteBy' test x (h:t) | test x h = t
                       | otherwise = h : deleteBy' test x t




-- g)

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' test [] = []
sortOn' test [x] = [x]
sortOn' test (x:y:t) | test x >  test y = y:sortOn' test (x:t)
                     | test x <= test y = x:sortOn' test (y:t)


-- 2 

type Polinomio = [Monomio]
type Monomio   = (Float,Int)

p' :: Polinomio
p' = [(2,3), (3,4), (5,3), (4,5)]
-- a)

selgrau' :: Int -> Polinomio -> Polinomio
selgrau' x l = filter (\(c,e) -> e == x) l



-- b) 

conta :: Int -> Polinomio -> Int  
conta x p = foldl (\c (a,b)  -> if b==x then c+1 else c) 0 p



-- c) 

grau :: Polinomio -> Int
grau p = foldl (\a (x,y)-> max y a) 0 p



-- d) 

deriv :: Polinomio -> Polinomio 
deriv p = map (\(x,y) -> (x*(fromIntegral y),y-1)) p



-- e)

calcula :: Float -> Polinomio -> Float
calcula x p = foldl (\ac (a,b) -> ac + ((a*x)^b)) 0 p



-- f)

simp :: Polinomio -> Polinomio
simp p = filter (\(a,b) -> a /= 0) p



-- g) 

mult :: Monomio -> Polinomio -> Polinomio
mult (c,d) p = map (\(a,b) -> (a*c,b+d)) p
mult' :: Monomio -> Polinomio -> Polinomio
mult' (c,d) p = foldl (\t (a,b) -> (c*a,d+b):t) [] p



-- k)

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\a m -> (mult m p1) ++ a) [] p2



-- l)

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = if (==) p1 p2 then True else False  



-- 3)
type Mat a = [[a]]


-- m1 = [[1,2,3], [0,4,5], [0,0,6]]

-- m2 = [[1,0,1],[2,2,4],[4,0,0]]



-- a)
dimOK :: Mat a -> Bool
dimOK  (a:b:t) | length a == length b = dimOK (a:t)
               | otherwise = False 



-- b)
dimMat :: Mat a -> (Int,Int)
dimMat (h:t) = (length h, length f)
   where f = (h:t)



-- c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m1 m2 = zipWith (zipWith (+)) m1 m2

addMat' [] [] = []
addMat' (x:xs) (y:ys) = (aux x y) : addMat' xs ys 
  where aux l1 l2 = zipWith (+) l1 l2



-- d)

transpose :: Mat a -> Mat a 
transpose ([]:x) = []
transpose l = (map head l) : transpose (map tail l)

-- e)
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat (h:t) m2 = linha h m2: multMat t (m2)
multMat [] m = []

linha :: Num a => [a] -> Mat a -> [a]
linha h ([]:_) =[] 
linha h m2 = sum (zipWith (*) h (map head m2)):linha h (map tail m2) 