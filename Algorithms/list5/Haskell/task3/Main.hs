module Main where

import HeapTest -- your test functions
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nStr, repsStr] -> do
      let maybeN = readMaybe nStr :: Maybe Int
          maybeReps = readMaybe repsStr :: Maybe Int
      case (maybeN, maybeReps) of
        (Just n, Just reps) -> do
          putStrLn $ "Running " ++ show reps ++ " experiments with n = " ++ show n
          runMultiple n reps
        _ -> usage
    [nStr] -> do
      let maybeN = readMaybe nStr :: Maybe Int
      case maybeN of
        Just n -> do
          putStrLn $ "Running 1 experiment with n = " ++ show n
          runExperiment n
        _ -> usage
    _ -> usage

usage :: IO ()
usage = putStrLn $ unlines
  [ "Usage:"
  , "  heaptest <n> [reps]"
  , "    n    - number of elements to insert in each heap"
  , "    reps - number of repeated experiments (optional, default=1)"
  ]
