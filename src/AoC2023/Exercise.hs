module AoC2023.Exercise (Exercise, resourceName, prettyName, solution, solve) where

import System.FilePath

class Exercise e where
    resourceName :: e -> FilePath
    prettyName :: e -> String
    solution :: e -> String -> String

solve :: Exercise e => e -> IO ()
solve e = do
    putStrLn $ "Running solution " ++ prettyName e ++ ":"
    putStrLn "--- Output below ---\n"

    exerciseInput <- readFile ("resources" </> resourceName e)
    putStrLn $ solution e exerciseInput
