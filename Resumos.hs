
import Data.Char
import Data.List
import System.Random


-- ========================================================================== --
-- Programação Funcional                                            2020/2021 --
-- Resumos                                                                    --
-- ========================================================================== --


-- -------------------------------------------- --
                -- ACUMULADORES --
-- -------------------------------------------- --


elemInd :: Eq a => Int -> a -> [a] -> [Int]
elemInd p x [] = []
elemInd p x (h:t) | x==h = p : elemInd (p+1) x t
                  | x/=h = elemInd (p+1) x t


elemInd' :: Eq a => a -> [a] -> [Int]
elemInd' x l = elemInd 0 x l  


----


soma' :: Num a => a -> [a] -> a
soma' acc [] = acc
soma' acc (h:t) = soma' (acc+h) t

soma :: Num a => [a] -> a
soma l = soma' 0 l


----


f0 :: [a] -> [a]
f0 [] = []
f0 (h:t) = h:f0 t


f1' :: [a] -> [a] -> [a]
f1' acc []    = acc
f1' acc (h:t) = f1' (acc ++ [h]) t

f1 :: [a] -> [a]
f1 l = f1' [] l 


----

reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = (last l) : reverse (init l)


revacc l = rev []

rev :: [a] -> [a] -> [a]
rev acc []    = acc
rev acc (h:t) = rev (h:acc) t










-- -------------------------------------------- --
                -- DIVISÃO E RESTO --
-- -------------------------------------------- --

myDiv :: Int -> Int -> Int
myDiv reb pess | pess>reb = 0
               | otherwise = 1 + myDiv (reb-pess) pess



myMod :: Int -> Int -> Int
myMod reb pess | pess>reb = reb
               | otherwise = myMod (reb-pess) pess



myDivMod :: Int -> Int -> (Int,Int)
myDivMod x y | y>x = (0,x)
             | otherwise = (1+d,m)
  where (d,m) = myDivMod (x-y) y



nzp :: [Int] -> (Int,Int,Int)
-- nzp l =! (length (filter (<0) l), length (filter (==0) l), length (filter (>0) l) )
nzp [] = (0,0,0)
nzp (h:t) | h<0   = (1+n,z,p) 
          | h== 0 = (n,1+z,p) 
          | h>0   = (n,z,1+p) 
    where (n,z,p) = nzp t


-- |Utilizando acumuladores
nzp1 :: [Int] -> (Int,Int,Int)
nzp1 l = nzpAc (0,0,0) l
--
nzpAc :: (Int,Int,Int) -> [Int] -> (Int,Int,Int)
nzpAc (n,z,p) [] = (n,z,p)
nzpAc (n,z,p) (h:t) | h<0  = nzpAc (1+n,z,p) t  
                    | h==0 = nzpAc (n,1+z,p) t
                    | h>0  = nzpAc (n,z,1+p) t



fromDigits :: [Int] -> Int
-- [1,4,0,3,5] -> 14035
fromDigits [] = 0
fromDigits (h:t) = h * 10^(length t) + fromDigits t 


-- | Usar acumulador para evitar ^ e length 
-- 5+10x (3+10x (0+10x (4+10x (1+10x0)))) = 14035
--
fromDigitsAc :: Int -> [Int] -> Int
fromDigitsAc ac [] = 0
fromDigitsAc ac (h:t) = fromDigitsAc (h+10*ac) t



toDigits :: Int -> [Int]
-- 14035 -- [1,4,0,3,5]
toDigits 0 = []
toDigits x | x<10 = [x]
           | x>10 = toDigits (div x 10) ++ [mod x 10]










-- -------------------------------------------- --
                -- ORDENAÇÃO DE LISTAS --
-- -------------------------------------------- --


-- Insertion Sort

-- função insert já definida na biblioteca
insert' :: (Ord a) => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) | x <= h = x:h:t
                | x >  h = h: insert' x t

iSort :: (Ord a) => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)



-- QuickSort

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (h:t) = (qsort p) ++ [h] ++ (qsort q)
   where (p,q) = partes h t

partes :: Ord a => a -> [a] -> ([a],[a])
partes x [] = ([],[])
partes x (h:t) | h <= x = (h:p,q)
               | h >  x = (p,h:q)
        where (p,q) = partes x t


-- MergeSort

-- merge -> junta listas ordenadas
merge :: Ord a => [a] -> [a] -> [a]
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys) 
         | x <  y = x: merge xs (y:ys)
         | x >= y = y: merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge l1' l2'
    where (l1,l2) = divide l --splitAt (div (length l) 2) l
          l1' = msort l1
          l2' = msort l2

--fazer a função splitAt
divide :: [a] -> ([a],[a])
divide [] = ([],[])
divide [x] = ([x],[])
divide (x:y:xs) = (x:xs1,y:xs2)
    where (xs1,xs2) = divide xs













-- -------------------------------------------- --
             -- FUNÇÕES ORDEM SUPERIOR --
-- -------------------------------------------- --

map' f l = [f x | x <- l]

dobros l = map f l
  where f x = 2*x

-- Como o 'x' repete na função é escusado por o 'x' 
-- ex. dobros' x = map (2*) x

dobros :: [Int] -> [Int]
dobros' = map (2*) 

pares :: [Int] -> [Int]
pares = filter even


filter' :: (a->Bool) -> [a] -> [a]
filter' teste [] = []
filter' teste (h:t) | (teste h)==True = h:filter' teste t
                    | otherwise = filter' teste t


---------------------
-- Função à parte que dá os nºs impares da lista
filtodd :: Integral a => [a] -> [a]
filtodd [] = []
filtodd (h:t) | (odd h) == True = h:filtodd t
              | otherwise = filtodd t
---------------------


takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' teste [] = []
takeWhile' teste (h:t) | (teste h) = h: takeWhile' teste t
                       | otherwise = []


dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' teste [] = []
dropWhile' teste (h:t) | (teste h) = dropWhile' teste t
                       | otherwise = (h:t)


zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y): zipWith' f xs ys

------


type Matriz = [Linha]
type Linha  = [Int]


-- somaM [] []         = [] 
-- somaM (x:xs) (y:ys) = somaLinhas x y : somaM xs ys
somaM :: Matriz -> Matriz -> Matriz
somaM m1 m2 = zipWith' somaLinhas m1 m2


-- somaLinhas [] [] = []
-- somaLinhas (x:xs) (y:ys) = (x+y):somaLinhas xs ys
somaLinhas :: Linha -> Linha -> Linha
somaLinhas = zipWith' (+) 


somaM' = zipWith' (zipWith' (+))



------
-- FOLDR FOLDL

precorre :: (a -> b -> b) -> b -> [a] -> b
precorre junta v [] = v
precorre junta v (h:t) = junta h (precorre junta v t)

-- precorre (+) 3 [1,2]
-- (+) 1 (precorre (+) 3 [2])
-- (+) 1 ((+) 2 (precorre (+) 3 []))
-- (+) 1 ((+) 2 3)

-- Em haskell a função precorre chama-se foldr
-- foldr (+) 3 [1,2] = 6


func l = foldr (:) [] -- função identidade

soma1 l = foldr (+) 0  l -- função soma

--soma1 [1,2,3,4] 
--   = 1 + (2 +(3 + (4 + 0)))



soma1' l = somaAcc 0 l 
somaAcc ac []    = ac
somaAcc ac (h:t) = somaAcc (ac+h) t 

--soma1' [1,2,3,4]
--   = (((0 + 1) + 2) + 3) + 4

precorreL :: (b -> a -> b) -> b -> [a] -> b
precorreL junta v [] = v
precorreL junta v (h:t) = precorreL junta (junta v h) t

-- precorreL (+) 0 [1,2]
-- precorreL ((+) 0 1) [2] 
-- precorreL ((+) ((+) 0 1) 2) []
-- ((+) \((+) 0 1) 2) = 3

-- Em haskell a função precorre chama-se foldl
-- foldl (+) 0 [1,2] = 3










-- -------------------------------------------- --
                -- ÁRVORES BINÁRIAS --
-- -------------------------------------------- --



-- data Lista a = Vazia | N a (Lista a)

-- comp :: Lista a -> Int
-- comp Vazia = 0
-- comp (N x xs) = 1 + comp xs


-- N 1 (N 2 (N 3 Vazia)) :: Lista Int 

-- Generalizando 

data ABin a  = Vazia
                | N a (ABin a) (ABin a)
        deriving Show


a1, a2, a3, a4, a5 , a6 :: ABin Int

a1 = Vazia
a2 = N 0 Vazia Vazia
a3 = N 5 (N 3 Vazia Vazia)
         (N 7 Vazia 
              (N 10 Vazia Vazia))

a4 = N 5 Vazia 
         (N 4 (N 1 Vazia Vazia) 
              (N 3 Vazia Vazia))

{-

                   10
              /         \   
           5              18
         /   \          /    \
        2     7       12      21
             / \             /  \
           6    8          19    35 
-}

a5 = N 10 (N 5 (N 2 Vazia Vazia) 
               (N 7 (N 6 Vazia Vazia)
                    (N 8 Vazia Vazia)))
          (N 18 (N 12 Vazia Vazia)
                (N 21 (N 19 Vazia Vazia)
                      (N 35 Vazia Vazia)))

a6 = N 100 a5 a3


comp :: ABin a -> Int
comp Vazia = 0
comp (N x y z) = 1 + (comp y) + (comp z)  


altura :: ABin a -> Int
altura Vazia = 0
altura  (N r e d) = 1 + max (altura e) (altura d)


mapABin :: (a -> b) -> (ABin a) -> (ABin b)
mapABin f Vazia = Vazia
mapABin f (N r e d) = N (f r) (mapABin f e) (mapABin f d)


------

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (h:t) | x == h = True
              | x /= h = elem' x t 

elemO :: Ord a => a -> [a] -> Bool
-- recebe uma lista ordenada por ordem crescente
elemO x [] = False
elemO x (h:t) | x == h = True
              | x <  h = False
              | x >  h = elemO x t 
------


elemA :: Eq a => a -> ABin a -> Bool
elemA x Vazia = False
elemA x (N r e d) | x == r = True
                  | x /= r = (elemA x e) || (elemA x d) 

-- Árvores de Procura
-- árvores ordenadas -> árvores binárias de procura
-- elementos à esquerda: todos menores ou iguais à raiz
-- elementos à direita: todos maiores ou iguais à raiz


procura :: Ord a => a -> ABin a -> Bool
-- recebe uma árvore de procura
procura x Vazia = False
procura x (N r e d) | x == r = True
                    | x >  r = procura x d
                    | x <  r = procura x e


acrescenta :: Ord a => a -> ABin a -> ABin a
-- recebe uma árvore de procura 
-- retorna uma árvore de procura
acrescenta x Vazia = (N x Vazia Vazia)
acrescenta x (N r e d) | x >= r = N r e (acrescenta x d)
                       | x <  r = N r (acrescenta x e) d 


fromList' :: Ord a => [a] -> ABin a
fromList' [] = Vazia
fromList' (h:t) = acrescenta h (fromList' t)


fromList'' l = foldr acrescenta Vazia l


maior :: ABin a -> a
-- recebe uma árvore de procura não vazia
maior (N r e Vazia) = r
maior (N r e d) = maior d


-------------------------------

-- Exercícios aula 

data BTree a = Empty | Node a (BTree a) (BTree a)
      deriving Show

arv1 :: Num a => BTree a
arv1 = Node 1 (Node 2 (Node 4 Empty Empty)
                      (Node 5 Empty Empty))
              (Node 3 Empty Empty)

--           1
--        2     3
--       4 5   


arv2 :: Num a => BTree a
arv2 = Node 1 (Node 2 (Node 4 Empty Empty)
                      (Node 5 Empty Empty))
              (Node 3 (Node 6 Empty Empty)
                      (Node 7 Empty Empty))



niveisBT :: BTree a -> [a]
niveisBT tree = travN [tree]


travN :: [BTree a] -> [a]
travN [] = []
travN (Empty:t) = travN t
travN ((Node r e d):t) = r : travN (t ++ [e,d])


---------


data RTree a = R a [RTree a]
    deriving Show


rt1 :: Num a => RTree a
rt1 = R 5 [R 4 [R 3 [R 17 []], R 2 [], R 7 []],
           R 10 [],
           R 1 [ R 8 [ R 0 [], R 20 [], R 15 [], R 39 [] ],
            R 12 [] ]
    ]  


niveisRT :: RTree a -> [a]
niveisRT (R r l) = r : auxRT l

auxRT :: [RTree a] -> [a]
auxRT [] = []
auxRT ((R r l):t) = r : auxRT (t ++ l)


--------

data LTree a = Tip a | Fork (LTree a) (LTree a)
      deriving Show


--       +
--      / \
--     8   +
--        / \
--       2   5

lt1 :: Num a => LTree a
lt1 = Fork (Tip 8) (Fork (Tip 2) (Tip 5))

travessia :: LTree a -> [(a,Int)]
travessia (Tip x) = [(x,1)]
travessia (Fork e d) = map (\(x,n)->(x,n+1)) (travessia e ++ travessia d) 

-- a função build, inversa desta, tal que build (travssia a) == a

build :: [(a,Int)] -> LTree a 
build l = fst (auxLT 1 l)



auxLT :: Int -> [(a,Int)] -> (LTree a,[(a,Int)])
auxLT n ((a,x):t) | n == x = (Tip a,t)
                  | n <  x = (Fork e d,t'')
                     where (e,t')  = auxLT (n+1) ((a,x):t)
                           (d,t'') = auxLT (n+1) t' 

--------------------------------










-- -------------------------------------------- --
                    -- CLASSES --
-- -------------------------------------------- --

{-
(+) :: Int -> Int -> Int -- muito restritiva

(+) :: a -> a -> a 

(+) :: Num a => a -> a -> a
       (tipo da função) a->a->a
       (tipo do tipo a) a é uma instância da classe Num
-}


-- Classes em Haskell

-------------------
-- 1. Definir uma classe

class NOME a where     -- a classe NOME é constituida pelos tipos a em que..
   -- restrições: funções que têm que existir  


-- class Eq a where       -- a classe Eq é constituída pelos tipos a em que...
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool 


-- 2. Definir instâncias

-- instance CLASSE Tipo where    -- Definir Tipo como um elemento de CLASSE

-- data BTree a = Empty | Node a (BTree a) (BTree a)
--      deriving Eq

-- o deriving Eq tem a mesma função que o instance em baixo

{-

-- fazer com que BTree a seja uma instância de Eq

instance (Eq a) => Eq (BTree a) where
    -- definir as funções (==) e (/=) com os tipos respetivos
    --(==) :: BTree a -> BTree a -> Bool
    Empty == Empty = True
    (Node r1 e1 d1) == (Node r2 e2 d2) =    (r1 == r2)
                                         && (e1 == e2)
                                         && (d1 == d2)
    _ == _ = False 
    --(/=) :: BTree a -> BTree a -> Bool
    a1 /= a2  = not (a1 == a2)

-}


x, y, z, w :: BTree Int
x = Empty
y = Node 3 Empty Empty
z = Node 4 (Node 5 Empty Empty) Empty
w = Node 6 (Node 42 Empty Empty) Empty
 
-- definição alternativa de BTree que verifica se a forma das BTrees são iguais
instance Eq (BTree a) where
    -- definir as funções (==) e (/=) com os tipos respetivos
    --(==) :: BTree a -> BTree a -> Bool
    Empty == Empty = True
    (Node r1 e1 d1) == (Node r2 e2 d2) = (e1 == e2) && (d1 == d2)
    _ == _ = False 
    --(/=) :: BTree a -> BTree a -> Bool
    a1 /= a2  = not (a1 == a2)



-- Classe Ord

data Time = AM Int Int 
          | PM Int Int
          | Total Int Int


-- Exemplo: Declarar Time como instância da classe Ord

totalmin :: Time -> Int
totalmin (AM h m)    = h*60 + m
totalmin (PM h m)    = (12+h)*60 + m
totalmin (Total h m) = h*60 + m

instance Eq Time where
  t1 == t2 = (totalmin t1) == (totalmin t2)

instance Ord Time where
   t1 <= t2 = (totalmin t1) <= (totalmin t2)



-- Classe Show

-- Exemplo: Declarar Nat como instância da classe Show
-- de forma a que os naturais sejam apresentados do modo usual

--data Nat = Zero | Suc Nat

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc n) = 1 + (natToInt n)

-- ex. Suc (Suc Zero) = 2

--instance Show Nat where
--     show n = show (natToInt n)
-- este é o show para o tipo Int. Entao Int é instância da classe Show


-- De outro modo pode ser definida assim:
-- onde show produz uma string com o mesmo aspeto do argumento

data Nat = Zero | Suc Nat
    deriving (Eq,Show)


-- Declarar Time como instância da classe Show

instance Show Time where
    show (AM h m) = (show h) ++ ":" ++ (show m) ++ " am"
    show (PM h m) = (show h) ++ ":" ++ (show m) ++ " pm"
    show (Total h m) = (show h) ++ "h" ++ (show m) ++ "m"




-- Classe Num


-- Exemplo: Nat como instância da classe Num


instance Num Nat where
    (+) = somaNat
    (*) = prodNat
    (-) = subNat
    fromInteger = deInteger
    abs = id
    signum = sinal
    negate n = error "indefinido ..."

somaNat :: Nat -> Nat -> Nat
somaNat Zero n  = n
somNat (Suc n) m = Suc (somaNat n m)


prodNat :: Nat -> Nat -> Nat
prodNat Zero _ = Zero
prodNat (Suc n) m = somaNat m (prodNat n m)

subNat :: Nat -> Nat -> Nat
subNat n Zero = n
subNat (Suc n) (Suc m) = subNat n m
subNat Zero _ = error "indefinido ..."

deInteger :: Integer -> Nat
deInteger 0 = Zero
deInteger n | n>0 = Suc (deInteger (n-1))
            | n<0 = error "indefinido ..."

sinal :: Nat -> Nat
sinal Zero = Zero
sinal (Suc _) = Suc Zero 



-- Classe Enum

-- Exemplo: Time como instância da classe Enum

instance Enum Time where
   toEnum n = let (h,m) = divMod n 60
              in Total h m
   fromEnum = totalmin


data Cor = Amarelo | Verde | Vermelho | Azul
  deriving (Enum, Show)






-- Classe Read

-- Classe Read estabelece funções usadas para converter uma string num valor

read :: Read a => String -> a
read s = case [x | (x,t) <- reads s, ("","") <- lex t] of
              [x] -> x
              [] -> error "Prelude.read: no parse"
              _   -> error "Prelude.read: ambiguous parse"

-- nao faço ideia do que isto seja










-- -------------------------------------------- --
                    -- IO --
-- -------------------------------------------- --


dialogo :: IO ()
dialogo = do putStr "Nome:"
             x <- getLine
             putStrLn ("Boa Tarde " ++ x) 


randomList :: Int -> (Int,Int) -> IO [Int]
-- randomList n (i,s) produz uma lista com n números entre i e s
randomList 0 (i,s) = return []
ranodmList n (i,s) = do x  <- randomRIO (i,s)
                        xs <- randomList (n-1) (i,s)
                        return (x:xs)

test :: IO ()
test = do putStrLn "Escreva uma frase: "
          l <- getLine
          let a = map toUpper l
              b = map toLower l
          putStrLn ("Maiúsculas: " ++a)
          putStrLn   ("Minúsculas: " ++b)

frase :: String -> IO String
frase s = do putStrLn s
             r <- getLine
             return r

questionario :: [String] -> IO [String]
questionario [] = return []
questionario (q:qs) = do r <- frase q
                         rs <- questionario qs
                         return (r:rs)


insereASorte :: a -> [a] -> IO [a]
insereASorte x [] = return [x]
insereASorte x l  = do p <- randomRIO (0,length l -1)
                       let (a,b) = splitAt p l
                       return (a ++ [x] ++ b)

-- permutacao :: [a] -> IO [a]
-- permutacao [] = return []
-- permutacao (h:t) = do x <- permutacao t
--                    insereASorte h x

wordCount :: String -> IO (Int,Int,Int)
wordCount nome = do conteudo <- readFile nome
                    let linhas = length $ lines conteudo
                        palavras = length $ words conteudo
                        caracteres = length conteudo
                    return (linhas,palavras,caracteres)

-- ========================================================================== --