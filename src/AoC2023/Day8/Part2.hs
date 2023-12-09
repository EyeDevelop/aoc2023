module AoC2023.Day8.Part2 (
    Day8Part2 (Day8Part2)
) where
import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (takeJust, Parser (parse))
import AoC2023.Day8.Part1 (Direction, Node (..), takeDirection, parseDirections)
import Data.List (isSuffixOf)

findNodesWith :: (Node -> Bool) -> [Node] -> [Node]
findNodesWith _ [] = []
findNodesWith f ((Node name leftName rightName):ns)
  | f (Node name leftName rightName) = Node name leftName rightName : findNodesWith f ns
  | otherwise = findNodesWith f ns

walk :: ([Direction], [Node]) -> [Int]
walk (directions, nodes) = [walkWith 0 node | node <- findNodesWith (\(Node name _ _) -> "A" `isSuffixOf` name) nodes]
  where
    nextDirection :: Int -> Direction
    nextDirection step = directions !! (step `mod` length directions)

    walkWith :: Int -> Node -> Int
    walkWith count (Node name leftName rightName)
      | "Z" `isSuffixOf` name = count
      | otherwise = walkWith (count + 1) (takeDirection nodes (nextDirection count) (Node name leftName rightName))

answer :: String -> String
answer = show . foldl1 lcm . walk . takeJust . parse parseDirections

data Day8Part2 = Day8Part2

instance Exercise Day8Part2 where
    resourceName _ = "day8p1.txt"
    prettyName _ = "Day 8: Part 2"
    solution _ = answer
