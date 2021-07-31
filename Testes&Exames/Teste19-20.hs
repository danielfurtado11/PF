-- Teste 2020

-- 1

-- a
intersect :: Eq a => [a] -> [a] -> [a]
intersect l l2 = [x | x <- l , elem x l2]

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] [] = []
intersect' (h:t) l | elem h l = h : intersect' t l
                   | otherwise = intersect' t l

intersect'' :: Eq a => [a] -> [a] -> [a]
intersect'' l1 l2 = filter (\x -> elem x l2) l1

-- b

tails :: [a] -> [[a]]

-- ex: tails [1,2,3]  -> [[1,2,3],[2,3],[3],[]]
tails [] = [[]]
tails l = l : tails (tail l) 

tails' :: [a] -> [[a]]
tails' l = [drop x l | x <- [0..length l]]


-- 2

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

-- a
elems :: ConjInt -> [Int]
elems [] = []
elems ((x,y):t) = [x..y]++(elems t)


elems' :: ConjInt -> [Int]
elems' l = concat $ map (uncurry enumFromTo) l


-- b
--
geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l = ((head (aux l)),(last (aux l))) : geraconj (drop (length (aux l)) l)


aux :: [Int] -> [Int]
aux (x:[]) = [x]
aux (x:y:t) | (y-x) == 1  = (x:aux (y:t))
            | otherwise = [head (x:y:t)]   

-- 3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
     deriving (Show)

type Nome = String
type Agenda = [(Nome, [Contacto])]


agenda = [("Manuela",[Email "manuela@gmail.com",Tlm 93444444445])]


-- a

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail x y [] = [(x,[Email y])]
acrescEmail x y ((a,b):t) | x==a = [(x,(Email y):b)] ++ t
                          | otherwise = (a,b) : acrescEmail x y t



-- b

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((x,cont):t) | n==x = Just (selectEmail cont)
                         | otherwise = verEmails n t


selectEmail :: [Contacto] -> [String]
selectEmail (Email x : t) = x : selectEmail t
selectEmail (h:t) = selectEmail t  


-- c
consulta :: [Contacto] -> ([Integer],[String])
consulta (Email x:t) = (a,x:b)
    where (a,b) = consulta t
consulta (Trab x:t) = (x:a,b)
    where (a,b) = consulta t
consulta (Tlm x:t) = (x:a,b)
    where (a,b) = consulta t
consulta (Casa x:t) = (x:a,b)
    where (a,b) = consulta t 


-- d

consultaIO :: Agenda -> IO ()
consultaIO x = do nome <- getLine
                  putStrLn (show (contact nome x))


contact :: Nome -> Agenda -> [Contacto]
contact n [] = []
contact n ((x,cont):t) | n==x = cont 
                       | otherwise = contact n t


-- 4

data RTree a = R a [RTree a]
  deriving (Show, Eq)

-- a

paths :: RTree a -> [[a]]
paths (R a []) = [[a]]
paths (R a l) =  map (a:) (concat (map (paths) l))




-- b

unpaths :: Eq a => [[a]] -> RTree a
unpaths a = (foldr (\x ac -> (lista x ac)) [] a)!!0



lista :: Eq a => [a] -> [RTree a] -> [RTree a]
lista [] xs = xs
lista (x:xs) [] = [R x (lista xs [])]
lista (x:xs) ((R a l):t) | x == a = t ++ [(R a (lista xs l))]
                         | otherwise = (lista (x:xs) t)++[(R a l)]











ar = R 1 [R 2 [],
          R 3 [R 4 [R 5 [],
                    R 6 []]],
          R 7 []]




consultaIO' :: Agenda -> IO ()
consultaIO' agenda = do x <- getLine 
                        putStrLn $ tiraCont agenda x


tiraCont :: Show a => Agenda -> Nome -> [Contacto]
tiraCont ((x,cont):t) n | n == x = cont
                        | otherwise = tiraCont t n