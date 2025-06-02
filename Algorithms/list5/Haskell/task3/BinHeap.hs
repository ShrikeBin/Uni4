module BinHeap where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Define a binomial tree node: rank, value, list of child trees
data BinomialTree a = Node Int a [BinomialTree a] deriving Show

-- Wrap the heap in a newtype for better encapsulation
newtype BinomialHeap a = BinomialHeap [BinomialTree a] deriving Show

-- Get the rank (degree) of a tree
rank :: BinomialTree a -> Int
rank (Node r _ _) = r

-- Get the root value of a tree
root :: BinomialTree a -> a
root (Node _ x _) = x

-- Get the child trees of a node
children :: BinomialTree a -> [BinomialTree a]
children (Node _ _ c) = c

-- Link two trees of the same rank: the smaller root becomes parent
link :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
    | x1 <= x2  = Node (r + 1) x1 (t2 : c1)  -- t2 becomes child of t1
    | otherwise = Node (r + 1) x2 (t1 : c2)  -- t1 becomes child of t2

-- Insert a value into the heap
insert :: Ord a => a -> BinomialHeap a -> BinomialHeap a
insert x (BinomialHeap ts) = BinomialHeap (insertTree (Node 0 x []) ts)

-- Insert a tree into a heap, ensuring no duplicate ranks
insertTree :: Ord a => BinomialTree a -> [BinomialTree a] -> [BinomialTree a]
insertTree t [] = [t]
insertTree t (t':ts)
    | rank t < rank t' = t : t' : ts
    | otherwise        = insertTree (link t t') ts  -- combine same-rank trees

-- Merge two heaps
merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge (BinomialHeap h1) (BinomialHeap h2) = BinomialHeap (mergeTrees h1 h2)

-- Merge the underlying tree lists
mergeTrees :: Ord a => [BinomialTree a] -> [BinomialTree a] -> [BinomialTree a]
mergeTrees [] h = h
mergeTrees h [] = h
mergeTrees h1@(t1:ts1) h2@(t2:ts2)
    | rank t1 < rank t2 = t1 : mergeTrees ts1 h2
    | rank t2 < rank t1 = t2 : mergeTrees h1 ts2
    | otherwise         = insertTree (link t1 t2) (mergeTrees ts1 ts2)

-- Find and remove the tree with the minimum root value
removeMinTree :: Ord a => [BinomialTree a] -> (BinomialTree a, [BinomialTree a])
removeMinTree [] = error "empty"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) =
    let (t', ts') = removeMinTree ts
    in if root t <= root t' then (t, ts) else (t', t : ts')

-- Extract the minimum value and return the new heap without it
extractMin :: Ord a => BinomialHeap a -> (a, BinomialHeap a)
extractMin (BinomialHeap ts) =
    let (Node _ x c, rest) = removeMinTree ts
        newHeap = merge (BinomialHeap (reverse c)) (BinomialHeap rest)
    in (x, newHeap)

-- COMPARISON COUNTING FUNCTIONS (renamed to avoid clash)

{-# NOINLINE comparisonCount #-}
comparisonCount :: IORef Int
comparisonCount = unsafePerformIO (newIORef 0)

resetComparisonCount :: IO ()
resetComparisonCount = writeIORef comparisonCount 0

incComparisonCount :: IO ()
incComparisonCount = modifyIORef' comparisonCount (+1)

getComparisonCount :: IO Int
getComparisonCount = readIORef comparisonCount
