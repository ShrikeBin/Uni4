module Nelly where

import Data.List (tails)

lmss' :: [Int] -> [Int]
lmss' (x:xs) = x : longest (map lmss' (snd (descTails (tails xs))))
    where
    longest = foldr (\y arr ->
        if length y >= length arr then y
        else arr
        ) []
    descTails = foldl (\(minHead, acc) arr -> case arr of
        [] -> (minHead, acc)
        (y:ys) ->
            if y > x && y < minHead then (y, acc ++ [y:ys])
            else (minHead, acc)
        ) (maxBound, []) 