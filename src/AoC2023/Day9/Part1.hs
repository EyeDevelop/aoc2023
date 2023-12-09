{-# LANGUAGE InstanceSigs #-}
module AoC2023.Day9.Part1 (
    Day9Part1 (Day9Part1),
    differences,
    extrapolate,
) where
import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (takeJust, Parser (parse), numberList)

differences :: [Int] -> [Int]
differences [] = []
differences [_] = []
differences (x:xs) = head xs - x : differences xs

extrapolate :: [Int] -> Int
extrapolate lst
  | all (0 ==) lst = 0
  | otherwise = last lst + extrapolate (differences lst)

answer :: String -> String
answer = show . sum . map (extrapolate . takeJust . parse numberList) . lines

data Day9Part1 = Day9Part1

instance Exercise Day9Part1 where
    resourceName :: Day9Part1 -> FilePath
    resourceName _ = "day9p1.txt"
    prettyName _ = "Day 9: Part 1"
    solution _ = answer
