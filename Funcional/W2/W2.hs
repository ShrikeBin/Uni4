module W2 where

id' x = x -- przypisuje typowi A funckję A -> A
inc x = x+1

integral :: Int -> Int -> (Double -> Double) -> Int
integral a b f
    |f 1 == 1 = b - a  -- dla funckji = 1
    |otherwise = 0     -- else

collapse :: Integer -> Integer
collapse n -- sławna 3n +1
    |n == 1 = 1
    |even n = collapse(div n 2) 
    |otherwise = collapse(3*n + 1)

collatz :: (Integer, Integer) -> (Integer, Integer)
collatz (n,s) -- śledzi liczbę iteracji
    |n == 1 = (1, s)
    |even n = collatz(div n 2, s + 1)
    |otherwise = collatz(3*n +1, s + 1)

--  wrapper na collapsa
coll n = snd(collatz(n,0)) -- snd (z Prelude) bierze drugi output

-- concat [[a]]->[a] złóż listy
-- "++" konkatenacja
concat' :: [[a]] -> [a]
concat' [] = []
concat' (i:arr) = i++concat' arr

-- zmierz rozmiar duuhh
length' :: [a] -> Integer
length' [] = 0
length' (i:arr) = 1 + length' arr
-- x:[1,2,3,4....] = [x,1,2,3,4.....] "wrzucenie z przodu"

head' :: [a] -> a
head' [] = error"head::pusta lista"
head' (i:_) = i

tail' :: [a] -> [a]
tail' [] = []
tail' (_:i) = i

-- nałóż funkcję na listę
map' :: (a ->b) -> [a] -> [b]
map' _ [] = []
map' f(i:arr) = f i:(map f arr)
-- użyj np map' (\x -> x^2) [1..10]

-- np filter' even [1..10]
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' condition (i:arr) = if condition i then i:(filter' condition arr)
                            else (filter' condition arr)

-- List Comprehension
-- [f a b c|a<-as, b<-bs, c<-cs, a < b < c]
-- trójki pitagorejskie <100 gcd - greatest common divisor
-- [(x, y, z) | z <-[1..100], y<-[1..z], x<-[1..y], x^2 + y^2 == z^2, gcd xy == 1]
