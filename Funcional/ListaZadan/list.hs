-- zad 4
sum' = \a b -> a+b
product' = \a b -> a*b


-- zad 8
integral :: Int -> Int -> (Double -> Double) -> Int
integral a b f
    |f 1 == 1 = b - a  -- dla funckji = 1
    |otherwise = 0     -- else


-- zad 10
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (i:arr) = f i:map' f arr

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (i:arr) (j:brr) = (i,j):zip' arr brr

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (i:arr) (j:brr) = f i j:zipWith' f arr brr

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (i:arr) = if f i then i:filter' f arr
                    else filter' f arr

take' :: Integer -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (i:arr) = i:take' (n-1) arr

drop' :: Integer -> [a] -> [a]
drop' 0 arr = arr
drop' _ [] = []
drop' n (_:arr) = drop' (n-1) arr


-- zad 13
helphi :: Integer -> Integer -> Integer
helphi n x
    |x == n = 0
    |gcd x n == 1 = 1 + helphi n (x+1)
    |otherwise = helphi n (x+1)

phi :: Integer -> Integer
phi 0 = 0
phi 1 = 0
phi 2 = 1
phi n = helphi n 2

-- suma phi(k) dla k = 1..n; k|n 
funfunc :: Integer -> Integer
funfunc n = sum[phi x | x <- [1..n], mod n x == 0]


-- zad 14
isperfect :: Integer -> Bool
isperfect n = n == sum [x | x <- [1..n-1], mod n x == 0]
-- filter' isperfect [1..10000]
-- [6,28,496,8128]

-- zad 15
sumDivisors :: Integer -> Integer
sumDivisors n = sum [x | x <- [1..floor (sqrt (fromIntegral n))], mod n x == 0, x /= n] + sumDivisorsHelper n

sumDivisorsHelper :: Integer -> Integer
sumDivisorsHelper n = sum [div n x | x <- [1..floor (sqrt (fromIntegral n))], mod n x == 0, div n x /= x, div n x /= n]

isfriend :: (Integer, Integer) -> Bool
isfriend (a, b) = sumDivisors a == b && sumDivisors b == a

--filter' isfriend [(x,y) | x<-[1..100000], y<-[1..100000]]
--[(6,6),(28,28),(220,284),(284,220),(496,496) ...
-- nie chce mi się czekać

-- zad 16
length' :: [a] -> Integer
length' [] = 0
length' (_:arr) = 1 + length' arr

dcp :: Integer -> Double
dcp n = fromIntegral (length' [(x,y) | x <- [1..n], y <- [1..n], gcd x y == 1]) / fromIntegral (n^2)

helpdcp :: Integer -> Integer -> Integer -> Integer
helpdcp n x y
    |x > n = 0 -- doszliśmy do końca pary na x
    |y > n = helpdcp n (x+1) 1 -- doszliśmy do końca pary na y
    |gcd x y == 1 = 1 + helpdcp n x (y+1) -- znaleźliśmy względnie pierwsze, odpal rekurencję
    |otherwise = helpdcp n x (y+1) -- nie były pierwsze -> idź dalej

-- XD policzył z 4gb stacka dla 10000
-- dodaj opcje: +RTS -K4000m -RTS
recdcp :: Integer -> Double
recdcp n = fromIntegral (helpdcp n 1 1) / fromIntegral (n^2)

-- map' recdcp ([100*x | x <- [1..50]])
-- daje jakieś ~0.608 zawsze potem