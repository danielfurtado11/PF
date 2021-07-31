-- Ficha 8

-- 1

-- b)

data Frac = F Integer Integer
    deriving Show

f = F (-33) (-51)
f2 = F 50 (-5)

normaliza :: Frac -> Frac
normaliza (F n d) = F (k * (div n' m )) (div d' m)
    where k = signum n * signum d
          m = mdc n' d'
          n' = abs n
          d' = abs d


mdc :: Integer -> Integer -> Integer
mdc a b
    | a > b = mdc (a-b) b
    | a < b = mdc a (b-a)
    | a==b = b



-- b)

instance Eq Frac 
   where (F a b) == (F c d) = a*d == c*d


-- c)

instance Ord Frac where
    f1 <= f2 =let (F a b) = normaliza f1
                  (F c d) = normaliza f2
              in a*d <= c*d