module AoC2023.Day9.Part1 (
    Day9Part1 (Day9Part1)
) where
import AoC2023.Exercise (Exercise (..))

answer :: String -> String
answer = show

data Day9Part1 = Day9Part1

instance Exercise Day9Part1 where
    resourceName _ = "day9p1.txt"
    prettyName _ = "Day 9: Part 1"
    solution _ = answer
