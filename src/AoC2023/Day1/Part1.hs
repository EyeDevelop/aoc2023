module AoC2023.Day1.Part1 where

import AoC2023.Exercise (Exercise (..))
import Data.Char (digitToInt, isDigit)

firstDigit :: String -> Char
firstDigit (x : xs)
  | isDigit x = x
  | otherwise = firstDigit xs

appendDigits :: String -> Int
appendDigits str = read [firstDigit str, firstDigit (reverse str)] :: Int

answer :: String -> String
answer = show . sum . map appendDigits . lines

data Day1Part1 = Day1Part1

instance Exercise Day1Part1 where
  resourceName _ = "day1p1.txt"
  prettyName _ = "Day 1: Part 1"
  solution _ = answer
