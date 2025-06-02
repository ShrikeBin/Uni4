module Notify where
    
import Data.Tree

-- Assume given tree in Data.Tree format
minRounds :: Tree a -> Int
minRounds (Node _ []) = 0
minRounds (Node _ children) =
    maximum $ zipWith (\order subtree -> minRounds subtree + order) [1..] sorted
  where
    sorted = reverse $ sortOn subtreeSize children
    subtreeSize (Node _ cs) = 1 + sum (map subtreeSize cs)

-- run exp
averageCase :: [Tree Int] -> (Double, Int, Int)
averageCase trees =
    let rounds = map minRounds trees
    in (average (map fromIntegral rounds), maximum rounds, minimum rounds)
  where
    average xs = sum xs / fromIntegral (length xs)
