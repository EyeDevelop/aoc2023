{-# LANGUAGE InstanceSigs #-}

module AoC2023.Day10.Part2
  ( Day10Part2 (Day10Part2),
  )
where

import AoC2023.Day10.Part1 (Direction (..), Grid, Pipe (..), Position, Tile (..), parseGrid, tileAt)
import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (Parser (parse), takeJust)

applyDirection :: Direction -> Position -> Position
applyDirection North (row, col) = (row - 1, col)
applyDirection South (row, col) = (row + 1, col)
applyDirection West (row, col) = (row, col - 1)
applyDirection East (row, col) = (row, col + 1)

isIncluded :: Grid -> Position -> Bool
isIncluded grid pos
  | tileAt grid pos /= Just Ground = False
  | otherwise = and [wallUp, wallDown, wallLeft, wallRight]
  where
    castRay :: (Tile -> Bool) -> Direction -> Position -> Bool
    castRay f dir current = case tileAt grid current of
      Just Ground -> castRay f dir (applyDirection dir current)
      Just tile -> f tile || castRay f dir (applyDirection dir current)
      Nothing -> False

    wallUp :: Bool
    wallUp = castRay (\tile -> tile `elem` [Pipe EastWest, Pipe ]) North pos

    wallDown :: Bool
    wallDown = castRay (Pipe EastWest ==) South pos

    wallLeft :: Bool
    wallLeft = castRay (Pipe NorthSouth ==) West pos

    wallRight :: Bool
    wallRight = castRay (Pipe NorthSouth ==) East pos

getNeighbours :: [Position] -> (Position -> Position) -> Position -> [Position]
getNeighbours groundPos f currentPos
  | currentPos `notElem` groundPos = []
  | otherwise = currentPos : getNeighbours groundPos f (f currentPos)

neighboursMatch :: [(Position, Bool)] -> Position -> Bool
neighboursMatch groundMap pos =
  let gridPositions = map fst groundMap
   in all (all (\neighbourPos -> (neighbourPos, True) `elem` groundMap)) [ getNeighbours gridPositions (applyDirection North) pos,
        getNeighbours gridPositions (applyDirection West) pos,
        getNeighbours gridPositions (applyDirection South) pos,
        getNeighbours gridPositions (applyDirection East) pos
      ]

answer :: String -> String
answer inp =
  let grid = takeJust $ parse parseGrid inp
      horizontalBound = length (head grid)
      verticalBound = length grid
      gridPositions = filter (\pos -> tileAt grid pos == Just Ground) [(row, col) | row <- [0 .. verticalBound - 2], col <- [0 .. horizontalBound - 1]]
      groundMap = map (\pos -> (pos, isIncluded grid pos)) gridPositions
   in show $ groundMap

data Day10Part2 = Day10Part2

instance Exercise Day10Part2 where
  resourceName _ = "day10p2.txt"
  prettyName _ = "Day 10: Part 2"
  solution _ = answer
