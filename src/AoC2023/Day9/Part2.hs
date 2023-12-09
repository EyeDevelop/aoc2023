module AoC2023.Day9.Part2 (
    Day9Part2 (Day9Part2)
) where
import AoC2023.Exercise (Exercise (..))
import AoC2023.Day9.Part1 (extrapolate)
import AoC2023.Util.Parser (takeJust, Parser (parse), numberList)

answer :: String -> String
answer = show . sum . map (extrapolate . reverse . takeJust . parse numberList) . lines

data Day9Part2 = Day9Part2

instance Exercise Day9Part2 where
    resourceName _ = "day9p1.txt"
    prettyName _ = "Day 9: Part 2"
    solution _ = answer
