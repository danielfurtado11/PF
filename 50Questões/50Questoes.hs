-- ========================================================================== --
-- Programação Funcional                                            2020/2021 --
-- Questões 1ª Parte                                                          --
-- ========================================================================== --

import Data.Char
import Data.List
import Data.Either

-- 1

myenumFromTo :: Int -> Int -> [Int]

myenumFromTo x y | x<y = x:(myenumFromTo (x+1) y)
                 | x==y = [x]
                 | otherwise = []



-- 2

myenumFromThenTo :: Int -> Int -> Int -> [Int]

myenumFromThenTo x y z | x==z = [x]
                       | otherwise = (x:enumFromThenTo y (y+(y-x)) z)



-- 3 (!!)

soma :: [a] -> [a] -> [a]

soma l [] = l
soma [] l = l 

soma (x:xs) l  = (x:soma xs l) 

-- [1,2,3] [10,20,30] --> [1,2,3,10,20,30]


-- 4

posicao :: [a] -> Int -> a

-- [10,20,30] 1 --> 20

posicao (h:t) 0 = h

posicao (h:t) x = posicao t (x-1)


-- posicao [1,2,3,4,5] 2 -- posicao [2,3,4,5] 1
                         -- posicao [3,4,5]   0
                         -- 3


-- 5

inverte :: [a] -> [a]

inverte [] = []
inverte (h:t) = inverte t ++ [h]


-- inverte [1,2,3] -- inverte [2,3] ++ [1]
                   -- inverte [3] ++ [2] ++ [1]
                   -- inverte [] ++ [3] ++ [2] ++ [1]
                   -- [3,2,1]

-- 6

mytake :: Int -> [a] -> [a]

-- 2 [1,2,3,4] -> [1,2]

mytake _ [] = []
mytake 0 l = []
mytake x (h:t) = (h:mytake (x-1) t)

-- mytake 2 [1,2,3,4,5] -- [1] ++ mytake 1 [2,3,4,5]
                        -- [1] ++ [2] ++ mytake 0 [3,4,5]
                        -- [1] ++ [2] ++ []
                        -- [1,2]


-- 7

mydrop :: Int -> [a] -> [a]

mydrop _ [] = []
mydrop 0 l  = l

mydrop x (h:t) = mydrop (x-1) t


-- 2 [1,2,4,5] -- drop 1 [2,4,5]
               -- drop 0 [4,5]
               -- [4,5]



-- 8

myzip :: [a] -> [b] -> [(a,b)]

myzip [] l = []
myzip l [] = []
myzip (x:xs) (y:ys) = ((x,y):myzip xs ys)


-- myzip [1,2,3] [1,2,3,4] -- [(1,1): zip [2,3] [2,3,4]]
                           -- [(1,1):(2,2): zip [3] [3,4]]
                           -- [(1,1):(2,2):(3,3):zip [] [4]]
                           -- [(1,1):(2,2):(3,3)]


-- 9

myelem :: Eq a => a -> [a] -> Bool

myelem x [] = False
myelem x (h:t) | x==h = True
               | otherwise = myelem x t



-- 10

replicar :: Int -> a -> [a]

replicar 0 _ = []

replicar x y = (y: replicar (x-1) y)


-- replicar 3 a -- (a:replicar 2 a)
                -- (a:a:replicar 1 a)
                -- (a:a:a: replicar 0 a)
                -- [a:a:a]




-- 11

intercala :: a -> [a]-> [a]

intercala _ [] = []
intercala _ [x] = [x]
intercala a (x:xs) = (x:a:intercala a xs)


-- 12 (!!)


agrupa :: Eq a => [a] -> [[a]]
agrupa [] = [[]]
agrupa [x] = [[x]]

agrupa (h:t) | h /= head(head x) = [h]:x 
             | otherwise = (h:head x):(tail x)
         where x = agrupa t


-- 13

concatena :: [[a]] -> [a]

concatena [] = []
concatena (x:xs) = x ++ concatena xs



-- [[1],[2,2],[3]] -- [1] ++ concatena [[2,2],[3]]
                   -- [1] ++ [2,2] ++ concatena [[3]]
                   -- [1] ++ [2,2] ++ [3] ++ concatena []
                   -- [1] ++ [2,2] ++ [3] ++ []
                   -- [1,2,2,3]




-- 14 (!!)

myinits :: [a] -> [[a]]

myinits [] = [[]]
myinits l = myinits (init l) ++ [l]


-- [11,21,12] -- myinits [11,21] ++ [11,21,12]
              -- myinits [11] ++ [11,21] ++ [11,21,12]
              -- myinits [] ++ [11] ++ [11,21] ++ [11,21,12]
              -- [[],[11],[11,21],[11,21,12]]


-- 15 

caudas :: [a] -> [[a]]

caudas [] = [[]]
caudas l = [l] ++ caudas (drop 1 l)


-- [11,21,12] -- [11,21,12] ++ caudas [21,12]
              -- [11,21,12] ++ [21,12] ++ caudas [12]
              -- [11,21,12] ++ [21,12] ++ [12] ++ caudas []
              -- [[11,21,12],[21,12],[12],[]]


-- 16

prefixo :: Eq a => [a] -> [a] -> Bool

prefixo [] l = True
prefixo (x:xs) (y:ys) | x==y = prefixo xs ys
                      | x/=y = False



-- 17

sufixo :: Eq a => [a] -> [a] -> Bool

sufixo [] l = True
sufixo x y | (last x) == (last y) = sufixo (init x) (init y)
           | (last x) /= (last y) = False

-- [20,30] [10,20,30] -- True




-- 18

sequencia :: Eq a => [a] -> [a] -> Bool

sequencia [] [] =  True
sequencia l [] = False
sequencia [] l = True
sequencia (x:xs) (y:ys) | x==y = sequencia xs ys
                        | x/=y = sequencia (x:xs) ys 


-- [20,30] [10,20,30,40]

-- sequencia [20,30] [20,30,40]
-- sequencia [30] [30,40]
-- sequencia [] [40]
-- True




-- 19 (!!)

aplista :: Eq a => a -> [a] -> [Int]

aplista x [] = []
aplista x l | x==last l = aplista x (init l) ++ [length l -1]
         | otherwise = aplista x (init l)


-- aplista 3 [1,3.2] -- ãplista 3 [1,3]
                     -- aplista 3 [1] ++ [1]
                     -- aplista 3 [] ++ [1]
                     -- [1]

p19 :: Eq a => a -> [a] -> [Int]
p19 x l = aux2 0 x l

aux2 :: Eq a => Int -> a -> [a]-> [Int]
aux2 ac x [] = []
aux2  ac x (h:t) | x == h = (ac: aux2 (ac+1) x t)
                 | otherwise = aux2 (ac+1) x t 



-- 20 

p20 :: Eq a => [a] -> [a]

p20 [] = []
p20 (x:xs) | (elem x xs)==True = p20 xs
           | otherwise = (x:p20 xs)

-- repete [1,2,1,2,3] -- repete [2,1,2,3]
                      -- repete [2,1,2,3]
                      -- repete [1,2,3]
                      -- (1: repete [2,3])
                      -- (1:2:repete [3])
                      -- (1:2:3:repete [])
                      -- [1,2,3]


-- 21
 
elimina :: Eq a => a -> [a] -> [a]

elimina x [] = []

elimina x (h:t) | x==h = t
                | otherwise =(h:elimina x t)



 -- elimina 2 [1,7,2,3,2] -- (1:elimina 2 [7,2,3,2])
                          -- (1:7:elimina 2 [2,3,2])
                          -- (1:7:[3,2])
                          -- [1,7,3,2]


-- 22

p22 :: Eq a => [a] -> [a] -> [a]

p22 l [] = l
p22 (x:xs) (y:ys) | x==y = p22 xs ys
                  | otherwise = (x: p22 xs (y:ys))

 

--p22 [1,2,3,4,5,1] [1,5] -- p22 [2,3,4,5,1] [5]
                          -- (2: p22 [3,4,5,1] [5])
                          -- (2:3: p22 [4,5,1] [5])
                          -- (2:3:4: p22 [5,1] [5])
                          -- (2:3:4: p22 [1] [])
                          -- [2,3,4,1]



 

 -- 23


uniao :: Eq a => [a] -> [a] -> [a]

uniao l [] = l
uniao l (y:ys) | (elem y l)==True = uniao l (ys)
               | otherwise = (uniao l ys) ++ [y]  


-- uniao [1,1,2,3,4] [1,5] -- uniao [1,1,2,3,4] [5]
                           -- uniao [1,1,2,3,4,5] []
                           -- [1,1,2,3,4,5]





-- 24


interseta :: Eq a => [a] -> [a] -> [a]

interseta [] l = []
interseta (x:xs) l | (elem x l)==True = (x: interseta xs l)
                   | otherwise = interseta xs l



-- interseta [1,1,2,3,4] [1,3,5] -- (1:interseta [1,2,3,4] [1,3,5])
                                 -- (1:1:interseta [2,3,4] [1,3,5])
                                 -- (1:1:interseta [3,4] [1,3,5])
                                 -- (1:1:3:interseta [4] [1,3,5])
                                 -- (1:1:3:interseta [] [1,3,5])
                                 -- [1,1,3]




-- 25

insere :: Ord a => a -> [a] -> [a]

insere x [] = [x]
insere x (h:t) | x>h = (h: insere x t)
               | x<=h = (x:h:t) 




-- insere 3 [1,2,4] -- (1: insere 3 [2,4])
                    -- (1:2: insere 3 [4])
                    -- (1:2:3:[4])
                    -- [1,2,3,4]






-- 26

myunwords :: [String] -> String

myunwords [] = ""
myunwords [x] = x
myunwords (x:t)= x++[' ']++myunwords t

-- ["a", "mi", "ta"] -- "a" ++ p26 ["mi","ta"]
                     -- "a" ++ "mi" ++ p26 ["ta"]
                     -- "a" ++ "mi" ++ "ta"
                     -- "a mi ta"

-- ["Programacao", "Funcional"] -- "Programacao Funcional"



-- 27

p27 :: [String] -> String

p27 [] = ""
p27 (h:t) = h++"\n"++(p27 t)


-- ["Prog", "Func"] "Prog\nFunc\n"



-- p28 (!!)

p28 :: Ord a => [a] -> Int


p28 (h:t) | h==aux (h:t) = 0
          | otherwise = 1+ p28 t
       where aux [x] = x
             aux (x:y:xs) | x>y = aux (x:xs)
                          | otherwise = aux (y:xs)

-- 

posicaos :: Ord a => [a] -> Int

posicaos [x] = 0
posicaos (h:t) | maximo (h:t)==h = 0
               | maximo (h:t)/=h = posicaos t +1 

maximo :: Ord a => [a] -> a

maximo [x] = x
maximo (x:y:t) | x>y = maximo (x:t)
               | x<y = maximo (y:t)



--

p223 :: Ord a => [a] -> Int
p223 l = acc 0 l 


acc :: Ord a => Int -> [a] -> Int
acc a [] = 0
acc a (h:t) | h == max1 (h:t) = a
            | otherwise = acc (a+1) t 

max1 :: Ord a => [a] -> a
max1 [x] = x
max1 (h:y:t) | h >= y = max1 (h:t) 
             | h <  y = max1 (y:t)


-- 29


repetidos :: Eq a => [a] -> Bool

repetidos [x] = False
repetidos (h:t) | (elem h t)==True = True
                | otherwise = repetidos t


-- repetidos [11,21,31,41] -- repetidos [21,31,41]
                           -- repetidos [31,41]
                           -- [repetidos [41]




-- 30


algarismos :: [Char] -> [Char]

algarismos [] = []
algarismos (h:t) | elem h num = (h:algarismos t)
                 | otherwise = algarismos t
       where num = ['0'..'9']

algarismos' :: [Char] -> [Char]
algarismos' [] = []
algarismos' (h:t) | isDigit h = (h:algarismos' t)
                  | otherwise = algarismos' t


-- 31

posimpar :: [a] -> [a]

posimpar [] = []
posimpar [x] = []
posimpar (x:y:xs) = (y:posimpar xs)



-- posimpar [1,2,3,4,5] -- (2:posimpar [3,4,5])
                        -- (2:4:posimpar [5])
                        -- (2:4:[])
                        -- [2,4]




-- 32

pospar :: [a] -> [a]

pospar [] =[]
pospar [x] = [x]
pospar (x:y:xs) = (x:pospar xs)



-- 33

ordenada :: Ord a => [a] -> Bool

ordenada [x] = True
ordenada (x:y:xs) | y>=x = ordenada (y:xs)
                  | otherwise = False


-- ordenada [1,2,3] -- ordenada [2,3]
                    -- ordenada [3]



-- 34


ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena (h:t) = insert h (ordena t)

-- ordena [1,4,3] -- insert 1 (ordena [4,3])
                  -- insert 1 (insere 4 (ordena (3)))
                  -- Insere 1 (insere 4 (insere 3 (ordena [])))
                  -- [1,3,4]


-- 35

menor :: String -> String -> Bool

menor [] x = True
menor x [] = False
menor (x:xs) (y:ys) | x<y  = True
                    | x>y  = False
                    | x==y = menor xs ys



-- 36 

elementos :: Eq a => a -> [(a,Int)] -> Bool

elementos a [] = False
elementos a ((x,y):xs) | a==x = True
                       | a/=x = elementos a xs




-- 37

tamanho :: [(a,Int)] -> Int
tamanho [] = 0
tamanho ((x,y):xs) = y + tamanho xs


-- 38

converte :: [(a,Int)] -> [a]
converte [] = []
converte ((a,b):xs) | b>0  = [a] ++ converte ((a,b-1):xs)
                    | b==0 = converte xs



-- 39 

p39 :: Eq a => a -> [(a,Int)] -> [(a,Int)]

p39 a [] = [(a,1)]
p39 a ((x,y):xs) | a==x = ((x,y+1):xs)
                 | a/=x = (x,y):p39 a xs



-- 40 

p40 :: Eq a => a -> [(a,Int)] -> [(a,Int)]

p40 a [] = []

p40 a ((x,y):xs) | a==x && y==1 = xs
                 | a==x && y>1  = ((x,y-1):xs)
                 | otherwise    = ((x,y): p40 a xs)



-- 41 (!!)

p41 :: Ord a => [a] -> [(a,Int)]

p41 [] = []
p41 (h:t) = (h,length (filter (==h) t)) : p41 (filter (/=h) t) 

-- [aaaabbc]




p41' :: Ord a => [a] -> [(a,Int)]

p41' [x] = [(x,1)]
p41' (h:t) =  [(h, conta h l)] ++ p41' (drop (conta h l) l)
        where l = (h:t)


conta :: Ord a => a -> [a] -> Int

conta x [] = 0
conta x (h:t) | x==h = (conta x t) +1
              | x/=h = conta x t


-- aaa -- 1 + conta a aa -- 1+1+ conta a a -- 1+1+1+ conta a [] = 3


-- [a,a,b,b,b] -- p41 [(a,2),(a,1)]




-- 42 (!!)

p42 :: [Either a b] -> ([a],[b]) 

p42 l = (left l,right l)
    where left (Left x :xs) = (x : left xs)
          left (Right x:xs) = left xs
          left _ = []
          right (Right x:xs) = (x : right xs)
          right (Left x:xs)  = right xs
          right _ = []




-- 43


maybes :: [Maybe a] -> [a]

maybes [] = []
maybes (Nothing : xs) = maybes xs
maybes (Just x: xs)   = (x:maybes xs)



----

data Movimento = Norte | Sul | Este | Oeste
               deriving Show

--44

p44 :: (Int,Int) -> [Movimento] -> (Int,Int)

p44 (x,y) [] = (x,y)
p44 (x,y) (Norte:t)= p44 (x,y+1) t
p44 (x,y) (Sul:t)  = p44 (x,y-1) t
p44 (x,y) (Este:t)= p44 (x+1,y) t
p44 (x,y) (Oeste:t)= p44 (x-1,y) t



-- p45

p45 :: (Int,Int) -> (Int,Int) -> [Movimento]


p45 (a,b) (c,d) | c>a = (Este: p45 (a+1,b) (c,d))
                | c<a = (Oeste: p45 (a-1,b) (c,d))
                | d>b  = (Norte: p45 (a,b+1) (c,d))
                | d<b  = (Sul: p45 (a,b-1) (c,d))
                | a==c && b==d = [] 



-- p46

p46 :: [Movimento] -> Bool

p46 [] = True
p46 (Norte:t) = p46 t
p46 (Sul:t)   = p46 t
p46 (Oeste:t) = False
p46 (Este:t)  = False


data Posicao = Pos Int Int
      deriving Show

-- p47 (!!)

central :: [Posicao] -> Posicao

central [x] = x
central (Pos a b:Pos c d:t) | (a^2 + b^2)>(c^2 + d^2) = central (Pos a b:t)
                            | otherwise = central (Pos c d:t) 



-- 48 

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos q i) [] = []
vizinhos (Pos x y) ((Pos a b):t) | x==(a+1) && y==b = m
                                 | x==(a-1) && y==b = m
                                 | x==a && y==(b+1) = m
                                 | x==a && y==(b-1) = m
                                 | otherwise = vizinhos (Pos x y) t
                           where m = (Pos a b:vizinhos (Pos x y)  t)



-- 49

mesmaOrd :: [Posicao] -> Bool 

mesmaOrd [x] = True
mesmaOrd ((Pos a b):(Pos c d):t) | b==d = mesmaOrd ((Pos a b):t)
                                 | otherwise = False



-- 50 (!!)

data Semaforo = Verde | Amarelo | Vermelho
               deriving Show



intersecao :: [Semaforo] -> Bool

intersecao l | aux l <= 1 = True
             | otherwise = False
          where aux [Verde]    = 1
                aux [Amarelo]  = 1
                aux [Vermelho] = 0
                aux (Vermelho:resto) = aux resto
                aux (Amarelo:resto)  = 1+ aux resto
                aux (Verde:resto)    = 1+ aux resto



                
---- 
grups :: Eq a => [a] -> [[a]] 
grups [] = []
grups [x] = [[x]]
grups (h:t) | h /= (head (head x)) = [h]:x
            | otherwise = (h:head x): (tail x)
      where x = grups t
