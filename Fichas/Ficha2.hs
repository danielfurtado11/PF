-- Exercício 2

-- a)

dobros :: [Float] -> [Float]

dobros [] = []
dobros (h:t) = (h * 2 : dobros t)


-- b)

type String = [Char]

numOcorre :: Char -> [Char] -> Int

numOcorre _ [] = 0
numOcorre x (h:t) | x == h = 1 + numOcorre x t
                  | otherwise = numOcorre x t


-- e)

somaNeg :: [Int] -> Int

somaNeg [] = 0
somaNeg (y:ys) | y < 0 = y + somaNeg ys 
               | otherwise = somaNeg ys



-- g)

segundos :: [(a,b)] -> [b]

segundos [] = []
segundos ((x,y):t) = (snd (x,y) : segundos t)


-- h) 

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool

nosPrimeiros _ [] = False 
nosPrimeiros x ((a,b):t) | x == a = True
                         | otherwise = nosPrimeiros x t 


-- i)

--sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)

--sumTriplos [] = (0,0,0)
--sumTriplos ((a,b,c):t) = (a+x, b+y, c+z)
--where (x,y,z) = sumTriplos t



-- Exercício 3

-- a)

isDigit :: Char -> Bool
isDigit x = x >= '0' && x <= '9'


soDigitos :: [Char] -> [Char]

soDigitos [] = []
soDigitos (h:t) | isDigit h = h : soDigitos t
                | otherwise = soDigitos t



-- Exercício 4

type Polinomio = [Monomio]
type Monomio = (Float, Int)


-- a)

conta :: Int -> Polinomio -> Int

conta x [] = 0

conta x ((a,b):t) | x==b      = 1 + conta x t
                  | otherwise = conta x t


-- b)

grau :: Polinomio -> Int

grau [] = 0
grau ((a,b):t) | b > grau t = b
               | otherwise = grau t  


--d)

deriv :: Polinomio -> Polinomio

deriv [] = []

deriv ((a,b):t) | a == 0 = deriv t
                | otherwise  = (a * (fromIntegral b), b-1) : deriv t


-- h)

somaM :: Polinomio -> Monomio -> Polinomio
-- assume que pol. não tem elols repetidos do mesmo grau
--produz um polinomio sem elos repetidos
-- [(2,3),(4,5)] (-2,5) -> [(2,3),(2,5)]

somaM [] m = [m]
somaM ((x,y):ms) (a,b) | y == b && x+a == 0 = ms
                      | y == b = (x+a,y):ms
                      | y /= b = (x,y):somaM ms (a,b)

normaliza :: Polinomio -> Polinomio

normaliza [] = []
normaliza (m:ms) = somaM ms' m
   where ms' = normaliza ms

-- k) 

somaMO :: Polinomio -> Monomio -> Polinomio
-- assume que o polinomio está ordenado
-- por ordem crescente o grau
-- sem repetições
-- produz um pol ordenado sem repetições

somaMO [] m = [m]

somaMO ((x,y):ms) (a,b) | b < y = (a,b):(x,y):ms
                         | b == y && x+a==0 = ms
                         | b == y           = (x+a,y):ms
                         | b > y            = (x,y):somaMO ms (a,b)

ordena :: Polinomio -> Polinomio

ordena [] = []
ordena (m:ms) = somaMO ms' m 
   where ms' = ordena ms

-- l)

equiv :: Polinomio -> Polinomio -> Bool

equiv x y = (ordena x) == (ordena y)
