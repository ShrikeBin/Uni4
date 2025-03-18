module W1 where

-- Silnia : IF ... ELSE ...
fact1 :: Integer -> Integer
fact1 n = if n==0 then 1
          else n * fact1 (n-1)

-- Silnia : pattern matching          
fact2 :: Integer -> Integer
fact2 0 = 1
fact2 n = n * fact2 (n - 1)

-- Silnia : case expresion
fact3 :: Integer -> Integer
fact3 n = case n of 
  0 -> 1
  n -> n * fact3 (n-1)

-- Silnia : with let expression  
fact4 :: Integer -> Integer
fact4 n  = let y = fact4 (n-1) in
           if n==0 then 1 else n * y