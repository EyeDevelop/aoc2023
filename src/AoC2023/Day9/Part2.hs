module AoC2023.Day9.Part2 (
    Day9Part2 (Day9Part2)
) where
import AoC2023.Exercise (Exercise (..))

answer :: String -> String
answer = show

data Day9Part2 = Day9Part2

instance Exercise Day9Part2 where
    resourceName _ = "day9p2.txt"
    prettyName _ = "Day 9: Part 2"
    solution _ = answer
