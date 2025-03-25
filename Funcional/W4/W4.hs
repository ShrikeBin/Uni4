module W4 where

import Data.List

sum' arr = foldl (+) 0 arr
product' arr = foldl (*) 1 arr
minimum' arr = foldl1 min arr
-- foldl1 bieże jako e(neutralny) 1'szy element z listy
-- itp

{-- Automaty Skończone --}

-- AUTOMAT DETERMINISTYCZNY
-- wyglada jak fold!

runDFA::(s -> c -> s) -> s -> [c] -> s
-- runDFA delta start cs = foldl delta start cs 
runDFA = foldl

-- automat na parzystą liczbę jedynek

delta 1 '1' = 2
delta 1 _ = 1
delta 2 '1' = 1
delta 2 _ = 2
delta _ _ = 1

--runDFA delta 1 "101" (policzy czy jest parzysta liczba 1)
{-
    -- nie skonczcone, jest na stronie
acceptDFA::(s -> c -> s) -> s -> (s -> Bool)-> [c] -> s
acceptDFA delta start cs = runDFA
-}

-- AUTOMAT NIEDETERMINISTYCZNY
-- nub - usuwa duplikaty


-- Nieskończone
myrepeat x = x:myrepeat x
repeat' x = xs where xs = x:xs

-- ...bedzie na stronie Cichonia

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate f (f x)
-- approx sqr (jakiś Newtonowski coś na obliczeniach naukowych)
approSqrt:: Double -> [Double]
approSqrt a = iterate (\x -> (x+a/x)/2) a

