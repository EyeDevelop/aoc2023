module AoC2023.Day10.Part2
  ( Day10Part2 (Day10Part2),
  )
where

import AoC2023.Day10.Part1 (Grid, Pipe (..), Position, Tile (..), parseGrid, tileAt)
import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (Parser (parse), takeJust)

intersections :: [Tile] -> Int
intersections [] = 0
intersections ((Pipe NorthSouth) : xs) = 1 + intersections xs
intersections ((Pipe NorthWest) : xs) = 1 + intersections xs
intersections ((Pipe NorthEast) : xs) = 1 + intersections xs
intersections (_ : xs) = intersections xs

isIncluded :: Grid -> Position -> Bool
isIncluded grid (row, col) = odd $ intersections (drop col (grid !! row))

-- Retrieve main loop
-- Everything that is not part of the main loop can be enclosed.

answer :: String -> String
answer inp =
  let grid = takeJust $ parse parseGrid inp
      horizontalBound = length (head grid)
      verticalBound = length grid
      gridPositions = filter (\pos -> tileAt grid pos == Just Ground) [(row, col) | row <- [0 .. verticalBound - 2], col <- [0 .. horizontalBound - 1]] --- So update this line.
   in show $ length $ filter (isIncluded grid) gridPositions

data Day10Part2 = Day10Part2

instance Exercise Day10Part2 where
  resourceName _ = "day10p2.txt"
  prettyName _ = "Day 10: Part 2"
  solution _ = answer
