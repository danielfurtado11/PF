-- FICHA 7


-- 1

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
    deriving (Show)


ex1:: ExpInt
ex1 = Mais (Const 3) (Mult (Const 7) (Simetrico (Const 5)))



-- a)

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = - (calcula e)
calcula (Mais a b)    = (calcula a) + (calcula b)
calcula (Menos a b)   = (calcula a) - (calcula b)
calcula (Mult a b)    = (calcula a) * (calcula b)



-- b)

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico e) = "- ( "++((infixa e))++" )"
infixa (Mais e1 e2)    = '(':' ':(infixa e1)++" + "++ (infixa e2)++" )"
infixa (Menos e1 e2)   = '(':' ':(infixa e1)++" - "++ (infixa e2)++" )"
infixa (Mult e1 e2)    = '(':' ':(infixa e1)++" * "++ (infixa e2)++" )"


-- c)

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e) = (posfixa e)++" ~"
posfixa (Mais e1 e2 ) = (posfixa e1)++' ':(posfixa e2) ++ " +"
posfixa (Menos e1 e2 ) = (posfixa e1)++' ':(posfixa e2) ++ " -"
posfixa (Mult e1 e2 ) = (posfixa e1)++' ':(posfixa e2) ++ " *"





-- 2

data RTree a = R a [RTree a] deriving Show
arv1 = R 8 [R 0 [],
            R 20 [],
            R 15 []] 



arv2 = R 5 [ R 4 [ R 3 [R 17 []], 
                  R 2 [], 
                  R 7 []],
            R 10 [], 
            R 1 [ R 8 [R 0 [],  
                       R 20 [],  
                       R 15 []], 
           
            R 12 [] ]]


-- a)

soma :: Num a => RTree a -> a 
soma (R a l) = a + sum (map soma l)


-- b)

altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l) = 1 + maximum (map altura l) 


-- c)

prune :: Int -> RTree a -> RTree a
prune  x (R a l) | x > 1  = R a (map (prune (x-1)) l)
                 | x == 1 = R a []


-- d)

mirror :: RTree a -> RTree a
mirror (R a l) = R a (reverse (map mirror l))



-- e)

postorder :: RTree a -> [a]
postorder (R a l) =  concat (map postorder l) ++ [a] 



-- 3)

data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

-- a)

ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a 
ltSum (Fork e d) = ltSum e + ltSum d


-- b)

listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d)



-- c)

ltHeight :: LTree a -> Int
ltHeight (Tip a) = 0
ltHeight (Fork e d) = 1 + (max (ltHeight e) (ltHeight d))



-- 4) 

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)


-- a)

splitFTree :: FTree a b -> (BTree a,LTree b)
splitFTree (Leaf x)   = (Empty, Tip x)
splitFTree (No n e d) = (Node n p1 p2, Fork q1 q2 )
  where (p1,q1) = splitFTree e
        (p2,q2) = splitFTree d



-- b)

-- joinTrees :: BTree a -> LTree a -> Maybe (FTree a b)

