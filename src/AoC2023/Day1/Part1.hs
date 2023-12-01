module AoC2023.Day1.Part1 where

import AoC2023.Exercise ( Exercise(..) )

answer :: String -> String
answer e = "Hello " ++ e

data Day1Part1 = Day1Part1
instance Exercise Day1Part1 where
    resourceName _ = "day1.txt"
    prettyName _ = "Day 1: Part 1"
    solution _ = answer
