import Data.List
import Data.Char
import System.Random
-- Teste 2018/2019

-- 1

-- a

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x [] = []
elemIndices' x l | x == (last l) = elemIndices' x (init l) ++ [length l -1]
                | x /= (last l) = elemIndices' x (init l) 

-- 2 [1,2,4,2,4,2] -> [1,3,5]



-- b

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False 
isSubsequenceOf' (x:xs) (y:ys) | x == y = isSubsequenceOf' xs ys
                              | x /= y = isSubsequenceOf' (x:xs) ys

--[20,40] [10,20,30,40,50]
--[40,20] [10,20,30,40]


-- 2

data BTree a = Empty | N a (BTree a) (BTree a)

-- a

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP x Empty = Nothing
lookupAP x (N (a,b) e d) | x == a = Just b
                         | x >  a = lookupAP x d
                         | x <  a = lookupAP x e  


-- b

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (N r e d) (N r2 e2 d2) = N (f r r2) (zipWithBT f e e2) (zipWithBT f d d2)



-- 3

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | (isDigit h == True) = (h:a,b)
                 | (isAlpha h == True) = (a,h:b)
                 | otherwise = (a,b)
            where (a,b) = digitAlpha t


-- 4

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)


-- a

firstSeq :: Seq a -> a
firstSeq (Cons a x) = a
firstSeq (App e d) = firstSeq e

-- b

dropSeq :: Ord a => Int -> Seq a -> Seq a
dropSeq _ Nil = Nil
dropSeq n (Cons a r) = dropSeq (n-1) r
dropSeq x (App e d)  | x > (contaSeq e) = dropSeq (x - contaSeq e) d
                     | x == contaSeq e = d
                     | otherwise = (App (dropSeq x e) d)


contaSeq :: Seq a -> Int
contaSeq Nil = 0
contaSeq (Cons a l) = 1+ contaSeq l
contaSeq (App e d) = contaSeq e + contaSeq d

ex1 = (App (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) (Cons 1 Nil)) 


-- c

instance Show a => (Show (Seq a)) where
     show a = "<<" ++ shows' a ++ ">>"

shows' :: Show a => Seq a -> String
shows' (App e d) = shows' e ++ "," ++ shows' d
shows' (Cons a e)= show a ++"," ++ shows' e
shows' (Nil) =  ""


-- 5

type Mat a = [[a]]

-- a

getElem :: Mat a -> IO a
getElem a = do x <- randomRIO (0,(length a -1))
               y <- randomRIO (0,(length (head a))-1)
               return ((a!!x)!!y)  

b :: Mat Int
b = [[6,7,2],[1,5,9],[8,3,4]]

magic :: Mat Int -> Bool

