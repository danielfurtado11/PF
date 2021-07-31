import Data.Char
import Data.List
-- Exame 2017 2018

-- 1

p1 :: [a] -> Int -> a

p1 (h:t) x | x == 0 = h
           | otherwise = p1 t (x-1)



-- 2

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:t) = posicao (x,y+1) t
posicao (x,y) (Sul:t)   = posicao (x,y-1) t
posicao (x,y) (Este:t)  = posicao (x+1,y) t
posicao (x,y) (Oeste:t) = posicao (x-1,y) t


-- 3

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) | (f h) == True = True
             | otherwise = any' f t 


-- 4

type Mat a = [[a]]

triSup :: (Eq a, Num a) => Mat a -> Bool

triSup m = triSupAc 1 m 

triSupAc ac [] = True
triSupAc ac (x:y:t) | lista0 (take ac y) == True  = triSupAc(ac+1)  t
                    | lista0 (take ac y) == False = False 
          where    lista0 [] = True
                   lista0 (h:t) | h == 0 = lista0 t
                                | otherwise = False


-- 5

moviementa :: IO (Int,Int)
moviementa = moviementa' (0,0) 

moviementa' :: (Int,Int) -> IO (Int,Int)
moviementa' (x,y) = do k <- getChar
                       case k of
                            'N' -> moviementa' (x,y+1)
                            'S' -> moviementa' (x,y-1)
                            'E' -> moviementa' (x+1,y)
                            'O' -> moviementa' (x-1,y)
                            otherwise -> return (x,y)

-- 6

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]
         deriving Show

-- a

vazia :: Imagem -> Bool
vazia (Quadrado x) = False
vazia (Juntar []) = True
vazia (Mover (x,y) i) = vazia i
vazia (Juntar l) | elem False (map vazia l) = False
                 | otherwise = True





-- b

maior :: Imagem -> Maybe Int 
maior l | maiores l == [] = Nothing
        | otherwise = Just (maximum (maiores l))


maiores :: Imagem -> [Int]
maiores (Juntar [])= []
maiores (Quadrado x)    = [x]
maiores (Mover (x,y) i)  = (maiores i)
maiores (Juntar (h:t))   = (maiores h) ++ (maiores (Juntar t)) 

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5), Quadrado 4, Mover (4,3) (Quadrado 2)])

ex1 :: Imagem
ex1 = Juntar [Mover (5,5) (Quadrado 4), Mover (5,6) (Quadrado 5), Mover (9,8) (Quadrado 2)] 


-- c

instance Eq Imagem where
    img1 == img2 = null $ (quad img1 (0,0)) \\ (quad img2 (0,0))


quad :: Imagem -> (Int,Int) -> [(Int,(Int,Int))]
quad (Quadrado n) pos = [(n,pos)]
quad (Mover (a,b) img) (x,y) = quad img (x+a,y+b)
quad (Juntar imgs) pos = concatMap (\x -> quad x (pos)) imgs 