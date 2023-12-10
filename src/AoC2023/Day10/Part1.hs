module AoC2023.Day10.Part1
  ( Day10Part1 (Day10Part1),
    Pipe (..),
    Tile (..),
    Grid,
    Position,
    Direction (..),
    tileAt,
    parseGrid,
    usePipe,
    findStart,
    constructLoop,
  )
where

import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (Parser (parse), delimited, literal, many, newline, takeJust)
import Control.Applicative (Alternative ((<|>)))
import Data.Function (on)
import Data.List (elemIndex, groupBy, sortOn)
import qualified Data.Ord as Ordering

data Pipe
  = NorthSouth
  | EastWest
  | NorthEast
  | NorthWest
  | SouthWest
  | SouthEast
  deriving (Eq, Show)

data Tile
  = StartPosition
  | Ground
  | Pipe Pipe
  deriving (Eq, Show)

type Grid = [[Tile]]

type Position = (Int, Int)

data Direction
  = North
  | South
  | West
  | East
  deriving (Eq, Show)

tileAt :: Grid -> Position -> Maybe Tile
tileAt grid (row, col) =
  if (row >= 0 && row < length grid - 1) && (col >= 0 && col < length (head grid))
    then Just (grid !! row !! col)
    else Nothing

parsePipe :: Parser Pipe
parsePipe =
  strToPipe
    <$> ( literal "|"
            <|> literal "-"
            <|> literal "L"
            <|> literal "J"
            <|> literal "7"
            <|> literal "F"
        )
  where
    strToPipe :: String -> Pipe
    strToPipe "|" = NorthSouth
    strToPipe "-" = EastWest
    strToPipe "L" = NorthEast
    strToPipe "J" = NorthWest
    strToPipe "7" = SouthWest
    strToPipe "F" = SouthEast
    strToPipe _ = error "No such pipe"

parseTile :: Parser Tile
parseTile =
  (pipeToTile <$> parsePipe)
    <|> (strToTile <$> (literal "." <|> literal "S"))
  where
    pipeToTile :: Pipe -> Tile
    pipeToTile = Pipe

    strToTile :: String -> Tile
    strToTile "S" = StartPosition
    strToTile "." = Ground
    strToTile _ = error "No such tile"

parseGrid :: Parser Grid
parseGrid = newline `delimited` many parseTile

findStart :: Grid -> Position
findStart = findRow 0
  where
    findRow :: Int -> [[Tile]] -> Position
    findRow _ [] = error "No start position!"
    findRow rowNum (row : rows) = case elemIndex StartPosition row of
      Just colNum -> (rowNum, colNum)
      Nothing -> findRow (rowNum + 1) rows

usePipe :: Pipe -> (Position, Direction) -> Maybe (Position, Direction)
usePipe pipe ((row, col), dir) = case (pipe, dir) of
  (NorthSouth, North) -> Just ((row - 1, col), North)
  (NorthSouth, South) -> Just ((row + 1, col), South)
  (EastWest, East) -> Just ((row, col + 1), East)
  (EastWest, West) -> Just ((row, col - 1), West)
  (NorthEast, South) -> Just ((row, col + 1), East)
  (NorthEast, West) -> Just ((row - 1, col), North)
  (NorthWest, South) -> Just ((row, col - 1), West)
  (NorthWest, East) -> Just ((row - 1, col), North)
  (SouthWest, North) -> Just ((row, col - 1), West)
  (SouthWest, East) -> Just ((row + 1, col), South)
  (SouthEast, North) -> Just ((row, col + 1), East)
  (SouthEast, West) -> Just ((row + 1, col), South)
  _ -> Nothing

constructLoop :: Grid -> Position -> [[(Position, Int)]]
constructLoop grid (row, col) =
    [ walkLoop ((row - 1, col), North) 1,
      walkLoop ((row + 1, col), South) 1,
      walkLoop ((row, col - 1), West) 1,
      walkLoop ((row, col + 1), East) 1
    ]
  where
    walkLoop :: (Position, Direction) -> Int -> [(Position, Int)]
    walkLoop (pos, dir) step
      | pos == (row, col) = []
      | otherwise =
          case tileAt grid pos of
            Just (Pipe p) -> case usePipe p (pos, dir) of
              Just nextPos -> (pos, step) : walkLoop nextPos (step + 1)
              _ -> []
            _ -> []

flattenWithLowest :: [[(Position, Int)]] -> [(Position, Int)]
flattenWithLowest lst = map (\xs@((key, _) : _) -> (key, minimum $ map snd xs)) ((groupBy ((==) `on` fst) . sortOn fst . concat) lst)

answer :: String -> String
answer inp =
  let grid = takeJust $ parse parseGrid inp
      start = findStart grid
   in show $ snd $ head $ sortOn (Ordering.Down . snd) $ flattenWithLowest $ constructLoop grid start

data Day10Part1 = Day10Part1

instance Exercise Day10Part1 where
  resourceName _ = "day10p1.txt"
  prettyName _ = "Day 10: Part 1"
  solution _ = answer
