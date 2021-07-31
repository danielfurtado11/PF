-- Ficha 3

-- 3 

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

c1,c2,c3 :: Contacto

c1 = Casa 1234
c2 = Tlm 438767
c3 = Email "jbb@uminho.pt"

a1,a2,a3 :: Agenda

a1 = [("abc",[c1,c3])]
a2 = [("def",[c2])]
a3 = [("Daniel",[c1,c2,c3])]


-- a)

acrescEmail :: Nome -> String -> Agenda -> Agenda

acrescEmail n m [] = [(n,[Email m])]
acrescEmail n m ((x,l):ag) | n == x = ((x,(Email m):l) :ag)
                           | otherwise = ((x,l): acrescEmail n m ag)

{-
-- b)

verEmails :: Nome -> Agenda -> Maybe [String]

verEmails n [] = Nothing
verEmails n ((x,l):ag) | n == x = Just selEmails l 
                       | otherwise = verEmails n ag
     where selEmails :: [Contacto] -> [String]
           selEmails [] = []
           selEmails ((Email m):cs) = m:selEmails cs
           selEmails (c:cs) = selEmails cs
-}





-- 4

type Dia = Int
type Mes = Int
type Ano = Int



data Data = D Dia Mes Ano
         deriving Show



type TabDN = [(Nome,Data)]


t1 :: TabDN

t1 = [("Joao", D 10 5 2001), ("Maria",D 4 10 1991),("Rui", D 3 3 1933)]



-- a)

procura :: Nome -> TabDN -> Maybe Data

-- procura "Joao" t1 == Just (D 10 5 2001)

procura x [] = Nothing

procura x ((a,b):t) | x==a = Just b 
                    | otherwise = procura x t

-- outra forma
procura1 :: Nome -> TabDN -> Maybe Data
procura1 x l = lookup x l



-- b)

idade :: Data -> Nome -> TabDN -> Maybe Int

idade h n t =
    case procura n t of 
        Nothing -> Nothing 
        Just d  -> Just (anos h d)


anos :: Data -> Data -> Int

anos (D dh mh ah) (D d m a) = if (mh>m || mh==m && dh>d)
                             then ah-a 
                             else ah-a-1


-- c)

anterior :: Data -> Data -> Bool

anterior (D d1 m1 a1) (D d2 m2 a2) = (a1<a2) || (a1==a2 && m1<m2) || (a1==a2 && m1==m2 && d1<d2)




-- d)

ordena :: TabDN -> TabDN

oredena [] = []
ordena (d:ds) = insert d (ordena ds)  
       where insert x [] = [x]
             insert x (y:ys) | anterior x y = x:y:ys
                             | otherwise    = y:insert x ys








iSort :: Ord a => [a] -> [a]

iSort [] = []
iSort (h:t) = insert h (iSort t)
        where insert x [] = []
              insert (n,d) ((n1,d1):ds) | anterior d d1 = (n,d):(n1,d1):ds
                                        | otherwise     = (n1,d1): insert (n,d) ds




































