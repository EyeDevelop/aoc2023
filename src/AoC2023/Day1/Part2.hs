module AoC2023.Day1.Part2
  ( Day1Part2 (Day1Part2),
  )
where

import AoC2023.Day1.Part1 (appendDigits)
import AoC2023.Exercise (Exercise (..))

-- Append last character back for parsing in case
-- a 'digit' has overlap.
-- Example: sevenine --> NOT 7ine but 79
parseLine :: String -> String
parseLine "" = ""
parseLine ('o' : 'n' : 'e' : xs) = '1' : parseLine ('e' : xs)
parseLine ('t' : 'w' : 'o' : xs) = '2' : parseLine ('o' : xs)
parseLine ('t' : 'h' : 'r' : 'e' : 'e' : xs) = '3' : parseLine ('e' : xs)
parseLine ('f' : 'o' : 'u' : 'r' : xs) = '4' : parseLine ('r' : xs)
parseLine ('f' : 'i' : 'v' : 'e' : xs) = '5' : parseLine ('e' : xs)
parseLine ('s' : 'i' : 'x' : xs) = '6' : parseLine ('x' : xs)
parseLine ('s' : 'e' : 'v' : 'e' : 'n' : xs) = '7' : parseLine ('n' : xs)
parseLine ('e' : 'i' : 'g' : 'h' : 't' : xs) = '8' : parseLine ('t' : xs)
parseLine ('n' : 'i' : 'n' : 'e' : xs) = '9' : parseLine ('e' : xs)
parseLine (x : xs) = x : parseLine xs

answer :: String -> String
answer = show . sum . map (appendDigits . parseLine) . lines

data Day1Part2 = Day1Part2

instance Exercise Day1Part2 where
  resourceName _ = "day1p1.txt"
  prettyName _ = "Day 1: Part 2"
  solution _ = answer
