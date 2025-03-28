module Stuff where

foo :: Integer -> Double
foo n = fromIntegral n * logBase 2 (fromIntegral n)

check :: Integer -> Integer -> Double
check n comps = fromIntegral comps / foo n
