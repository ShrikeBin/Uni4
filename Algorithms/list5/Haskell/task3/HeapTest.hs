module HeapTest where

import BinHeap
import System.Random
import Control.Exception (catch, SomeException, evaluate)
import Data.List (foldl')
import System.IO
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- A wrapper around values that counts comparisons
newtype Counted a = Counted a deriving (Eq, Show)

-- Reset counter
resetCount :: IO ()
resetCount = writeIORef comparisonCount 0

-- Increment counter
incCount :: IO ()
incCount = modifyIORef' comparisonCount (+1)

-- Extract counter
getCount :: IO Int
getCount = readIORef comparisonCount

-- Ord instance that counts comparisons
instance Ord a => Ord (Counted a) where
    compare (Counted x) (Counted y) = unsafePerformIO $ do
        incCount
        return (compare x y)

-- Underlying value extraction
unwrap :: Counted a -> a
unwrap (Counted x) = x

-- Insert multiple elements
insertAll :: Ord a => [Counted a] -> BinomialHeap (Counted a) -> IO (BinomialHeap (Counted a), [Int])
insertAll xs heap = go xs heap []
  where
    go [] h acc = return (h, reverse acc)
    go (x:xs') h acc = do
        resetCount
        let h' = insert x h
        c <- getCount
        go xs' h' (c : acc)

-- ExtractMin all elements (2n times)
extractAll :: Ord a => Int -> BinomialHeap (Counted a) -> IO ([Counted a], [Int])
extractAll 0 h = return ([], [])
extractAll n h = do
    resetCount
    let (x, h') = extractMin h
    c <- getCount
    (xs, cs) <- extractAll (n-1) h'
    return (x:xs, c:cs)

-- Generate n random values wrapped as Counted
genRandomList :: Int -> IO [Counted Int]
genRandomList n = do
    g <- newStdGen
    return $ map Counted $ take n (randomRs (1,1000000) g)

-- Check if list is sorted
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Run single experiment for given n
runExperiment :: Int -> IO ()
runExperiment n = do
    putStrLn $ "Running experiment with n = " ++ show n
    xs1 <- genRandomList n
    xs2 <- genRandomList n

    -- Create empty heaps
    let hEmpty = BinomialHeap []

    -- Insert into two heaps, collecting comparisons
    (h1, insComp1) <- insertAll xs1 hEmpty
    (h2, insComp2) <- insertAll xs2 hEmpty

    -- Merge heaps
    resetCount
    let hMerged = merge h1 h2
    mergeComp <- getCount

    -- Extract min 2n times, collecting comparisons
    (extracted, exComp) <- extractAll (2*n) hMerged

    -- Check sortedness and emptiness
    putStrLn $ "Is extracted list sorted? " ++ show (isSorted extracted)

    emptyAfter <- case extracted of
        [] -> return False
        _  -> (evaluate (extractMin hMerged) >> return False)
              `catch` \(_ :: SomeException) -> return True
    putStrLn $ "Is heap empty after extraction? " ++ show emptyAfter

    -- Save comparison counts to CSV files
    let insCsv = "insert_comparisons_n" ++ show n ++ ".csv"
        exCsv  = "extract_comparisons_n" ++ show n ++ ".csv"

    writeFile insCsv $ unlines $ map show (insComp1 ++ insComp2)
    writeFile exCsv $ unlines $ map show exComp

    -- Print summary
    let totalIns = sum insComp1 + sum insComp2
        totalEx  = sum exComp
        totalOps = 2 * n
        avgOps  = fromIntegral (totalIns + totalEx + mergeComp) / fromIntegral totalOps

    putStrLn $ "Total insert comparisons: " ++ show totalIns
    putStrLn $ "Total merge comparisons: " ++ show mergeComp
    putStrLn $ "Total extract comparisons: " ++ show totalEx
    putStrLn $ "Average comparisons per operation: " ++ show avgOps

-- Run multiple experiments for fixed n
runMultiple :: Int -> Int -> IO ()
runMultiple n reps = mapM_ (\i -> putStrLn ("\nExperiment " ++ show i) >> runExperiment n) [1..reps]

-- Run experiments over a list of n values and save total/n average in CSV
runScaling :: [Int] -> IO ()
runScaling ns = do
    results <- mapM (\n -> do
        resetCount
        xs1 <- genRandomList n
        xs2 <- genRandomList n
        let hEmpty = BinomialHeap []
        (h1, _) <- insertAll xs1 hEmpty
        (h2, _) <- insertAll xs2 hEmpty
        resetCount
        let hMerged = merge h1 h2
        mergeComp <- getCount
        (extracted, exComp) <- extractAll (2*n) hMerged
        let totalIns = 0 -- insert counts omitted here for simplicity
            totalEx  = sum exComp
            totalComp = totalIns + totalEx + mergeComp
            avgComp = fromIntegral totalComp / fromIntegral n
        putStrLn $ "n = " ++ show n ++ ", avg comparisons per op = " ++ show avgComp
        return (n, avgComp)
        ) ns

    let csvFile = "scaling_results.csv"
    writeFile csvFile $ unlines $ ("n,avg_comp" : map (\(n,c) -> show n ++ "," ++ show c) results)
