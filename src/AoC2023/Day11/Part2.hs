module AoC2023.Day11.Part2 (
    Day11Part2 (Day11Part2)
) where
import AoC2023.Exercise (Exercise (..))

answer :: String -> String
answer _ = ""

data Day11Part2 = Day11Part2

instance Exercise Day11Part2 where
    resourceName _ = "day11p1.txt"
    prettyName _ = "Day 11: Part 2"
    solution _ = answer