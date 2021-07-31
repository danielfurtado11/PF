-- FICHA 6

-- 1)

data BTree a = Empty
             | Node a (BTree a) (BTree a)
       deriving Show


-- a) 

altura :: BTree a -> Int
altura Empty = 0
altura (Node r e d) = 1 + max (altura e) (altura d)


-- b)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d) = 1 + (contaNodos e) + (contaNodos d)


-----
a4 :: Num a => BTree a
a4 = Node 5 Empty (Node 4 (Node 1 Empty Empty) (Node 3 Empty Empty))

--               5
--                 \ 
--                  4
--                 / \
--                1   3
-----
a5 = Node 10 (Node 5 (Node 2 Empty Empty) 
               (Node 7 (Node 6 Empty Empty)
                    (Node 8 Empty Empty)))
          (Node 18 (Node 12 Empty Empty)
                (Node 21 (Node 19 Empty Empty)
                      (Node 35 Empty Empty)))


--                   10
--              /         \   
--           5              18
--         /   \          /    \
--        2     7       12      21
--             / \             /  \
--           6    8          19    35 



a1 = Node 1 (Node 2 Empty Empty)
            (Node 3 Empty Empty)

-- c)

folhas ::BTree a -> Int
folhas (Node r Empty Empty) = 1
folhas (Node r e d)  = (folhas e) + (folhas d)
folhas Empty = 0



-- d)

prune :: Int -> BTree a -> BTree a
prune 0 (Node r e d) = Empty
prune x Empty        = Empty 
prune x (Node r e d) = Node r (prune (x-1) e) (prune (x-1) d) 


-- e) 

path :: [Bool] -> BTree a -> [a]
path [] (Node r e d) = [r]
path [] Empty = []
path (False:t) (Node r e d) = [r] ++ (path t e)
path (True:t) (Node r e d)  = [r] ++ (path t d)



-- f) 

mirror :: BTree a -> BTree a 
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)


-- g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2)
       = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT f Empty  _ = Empty
zipWithBT f _ Empty  = Empty


-- h) 

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (r1,r2,r3) e d)  = (Node r1 e1 d1,Node r2 e2 d2,Node r3 e3 d3)
   where (e1,e2,e3) = unzipBT e
         (d1,d2,d3) = unzipBT d



-- 2

-- a) 

minimo :: Ord a => BTree a -> a
minimo (Node r Empty Empty) = r
minimo (Node r e d) = minimo e



-- b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty d) = Empty
semMinimo (Node r e d) = (Node r (semMinimo e) d)



-- c) 

minSmin :: Ord a => BTree a ->  (a,BTree a)
minSmin (Node r Empty d) = (r,d)
minSmin (Node r e d) = (x,(Node r y d))
  where (x,y) = minSmin e



-- d)

remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty 
remove x (Node r e d) | x > r  = Node r e (remove x d)
                      | x < r  = Node r (remove x e) d
                      | x == r = Node m e a
         where (m,a) = minSmin d



-- 3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
      deriving Show

type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

a :: Aluno
a = (1,"Manel", ORD, Aprov 17)
a2 = (4,"Manela", ORD,Rep)
a3 = (44,"Carlos",TE,Aprov 13)
a' = (24,"Miguel",TE,Aprov 8)
ar = Node a2 (Node a Empty Empty) (Node a' Empty (Node a3 Empty Empty))



-- a)

inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node (num,nm,r,cl) e d) 
                | n == num = True
                | otherwise = (inscNum n e) || (inscNum n d)



-- b)

inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (num,nm,r,cl) e d) 
              | n == nm = True
              | otherwise = (inscNome n e) || (inscNome n d)



-- c)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nm,TE,cl) e d) = trabEst e ++ [(num,nm)] ++ trabEst d
trabEst (Node (num,nm,_,cl) e d)  = trabEst e ++ trabEst d


-- d)

nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota n (Node (num,nm,r,cl) e d) | n == num = Just cl 
                                | n >  num = nota n d
                                | n <  num = nota n e


-- e)  

contaFaltas :: Turma -> Float
contaFaltas Empty = 0
contaFaltas (Node (num,nm,r,Faltou) e d) = 1 + contaFaltas e + contaFaltas d
contaFaltas (Node (num,nm,r,_) e d) = contaFaltas e + contaFaltas d

numAlunos :: Turma -> Float 
numAlunos (Node (num,nm,r,cl) e d) = 1 + numAlunos e + numAlunos d
numAlunos Empty = 0

percFaltas :: Turma -> Float
percFaltas x = ((contaFaltas x) * 100) / numAlunos x 



-- f)

listaMed:: Turma -> [Int]
listaMed Empty = []
listaMed (Node (num,nm,r,Aprov x) e d) = (x:listaMed e) ++ (listaMed d)
listaMed (Node (num,nm,r,_) e d) = listaMed e ++ listaMed d

mediaAprov :: Turma -> Float
mediaAprov x = ((fromIntegral(somaMed (listaMed x))) / fromIntegral(length (listaMed x)))
 
 
somaMed :: [Int] -> Int
somaMed [] = 0
somaMed (h:t) = h + somaMed t


-- g)

conta :: Turma -> (Int,Int)
conta Empty = (0,0) 
conta (Node (num,nm,r,Aprov x) e d) = (1+p+p1,q+q1)
     where (p,q) = conta e
           (p1,q1) = conta d
conta (Node (num,nm,r,Rep) e d)     = (p+p1,1+q+q1)
     where (p,q) = conta e
           (p1,q1) = conta d
conta (Node (num,nm,r,_) e d)       = (p+p1,q+q1)
     where (p,q) = conta e
           (p1,q1) = conta d

aprovAv :: Turma -> Float
aprovAv x = (fromIntegral a) / (fromIntegral b)
   where (a,b) = conta x 