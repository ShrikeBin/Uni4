module MST where

import System.Random
import Data.List
import qualified Data.Set as Set

type Edge = (Int, Int, Double)
type Graph = [Edge]

-- Generate a random graph with n vertices
generateGraph :: Int -> IO Graph
generateGraph n = do
    weights <- sequence [randomRIO (0,1) | _ <- [1..(n*(n-1) `div` 2)]]
    let edges = [(i, j, w) | (i, j, w) <- zip3 [i | i <- [0..n-1], j <- [i+1..n-1]]
                                           [j | i <- [0..n-1], j <- [i+1..n-1]] weights]
    return edges

-- Kruskal
kruskal :: Int -> Graph -> [Edge]
kruskal n edges = kruskal' (sortOn (\(_,_,w) -> w) edges) (replicate n [0 .. n-1])
  where
    findSet sets v = head [s | s <- sets, v `elem` s]
    unionSets sets u v = let su = findSet sets u
                             sv = findSet sets v
                             merged = su ++ sv
                         in merged : filter (\s -> s /= su && s /= sv) sets
    kruskal' [] _ = []
    kruskal' ((u,v,w):es) sets
      | findSet sets u /= findSet sets v = (u,v,w) : kruskal' es (unionSets sets u v)
      | otherwise = kruskal' es sets

-- Prim
prim :: Int -> Graph -> [Edge]
prim n edges = prim' Set.empty [0] []
  where
    adj u = [(v,w) | (x,v,w) <- edges, x == u] ++ [(u,w) | (u,x,w) <- edges, x == u]
    prim' visited frontier mst
      | length visited == n = mst
      | otherwise =
          let options = [(u,v,w) | u <- frontier, (v,w) <- adj u, not (Set.member v visited)]
              (u,v,w) = minimumBy (\(_,_,w1) (_,_,w2) -> compare w1 w2) options
          in prim' (Set.insert v visited) (v : frontier) ((u,v,w) : mst)
