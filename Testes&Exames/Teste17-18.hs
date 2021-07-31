-- Teste 2017/2018

-- 1

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) | x >  h = insert x t
               | x <= h = (x:h:t)

-- 2

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x:t) = x : catMaybes t
catMaybes (Nothing:t) = catMaybes t

-- 3

data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)


instance Show a => (Show (Exp a)) where
      show = show' 


show' :: Show a => Exp a -> String
show' (Const a) = show a 
show' (Var x)   = x 
show' (Mais a b)  ="("++show a++" + "++show b++")"
show' (Mult a b)  ="("++show a++" * "++show b++")"

rr = show (Mais (Var "x") (Mult (Const 3) (Const 4)))

-- 4

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [x] = [x]
sortOn' f (x:y:t) | (f x) < (f y) = (x:sortOn' f (y:t))
                  | (f x) > (f y) = (y:sortOn' f (x:t)) 


-- 5

-- a

amplitude :: [Int] -> Int
amplitude l = (maximum l) - (minimum l)

amplitude' :: [Int] -> Int
amplitude' l = mx - mn
   where (mx,mn) = foldl (\(a,b) n -> (if n > a then n else a, if n < b then n else b)) (head l, head l) l

-- b


