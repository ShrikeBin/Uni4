module W3 where

-- do testu i wykładu, data char strumienie tekstowe
import Data.List (transpose, partition, permutations)
--import Data.Char


--  Bad Quick Sort (list comprehension)
bqS [] = []
bqS (i:arr) = (bqS [x | x <- arr, x < i]) ++ 
            [i] ++
            (bqS [x | x <- arr, x >= i])


-- Quick Sort (a better one)
qS [] = []
qS (i:arr) = (qS l) ++ [i] ++ (qS r)
            where (l, r) = partition (<i) arr

{-
-- Co nam potrzebne? (używamy wbudowanego)
-- partition (AAA TO TAK TO WHERE DZIAŁA), ale da się szybciej
partition::(a->Bool)->[a]->([a],[a])
partition _ [] = ([],[])
partition p (i:arr) = if p i then (i:l, r) 
                            else (l, i:r)
                            where (l, r) = partition p arr
-}

-- Insertion Sort
inSort [] = []
inSort (x:xs) = l ++ [x] ++ r
                where   sxs = inSort xs 
                        (l, r) = partition (<x) sxs


-- zip, zipWith
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (i:arr) (j:brr) = (i,j):zip' arr brr

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (i:arr) (j:brr) = f i j:zipWith' f arr brr

-- :sprint -> do sprawdzania ewaluacji
{-
λ> xx = [3,4,5,1,2,8,9,1,2]
λ> zipWith (-) (tail xx) xx
[1,1,-4,1,6,1,-8,1]

λ> zipWith (zipWith (+)) [[1,1],[1,1]] [[2,3],[4,5]]
[[3,4],[5,6]]

-}

-- idk co xd
add [] = 0
add (i:arr) = i + add arr

prod [] = 1
prod (i:arr) = i * prod arr


-- funkcja na łączność, prawo-lewo stronną e to starter (i element neutralny działania)
myFoldRight op e [] = e
myFoldRight op e (i:arr) = op i (myFoldRight op e arr)

myFoldLeft op e [] = e
myFoldLeft op e (i:arr) = myFoldLeft op (op e i) arr

{-
niech x # y = y + x

wtedy foldleft # e arr = foldright + e (reverse arr)

foldleft:       foldright:
    /\x4               x1/\
   /\x3                 x2/\
  /\x2                   x3/\                              
e/\x1                     x4/\e        

gdy wezmiemy operację ':' - składania
oraz (flip f) x y = f y x
to
reverse arr = foldleft(flip :) [] arr odwracanie linowe
-}