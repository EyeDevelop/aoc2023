module AoC2023.Day3.Part1
  ( Day3Part1 (Day3Part1),
    Position,
    Element (..),
    GridElement,
    GridRow,
    Grid,
    elementAt,
    parseGrid,
    positionsAround,
    isGridNumber,
  )
where

import AoC2023.Exercise (Exercise (..))
import Data.Char (isDigit)

type Position = (Int, Int)

data Element = GridNumber String | GridNumberTail Position | GridSymbol Char | GridEmpty
  deriving (Show)

type GridElement = (Element, Position)

type GridRow = [GridElement]

type Grid = [GridRow]

type GridBounds = (Int, Int)

elementAt :: Grid -> Position -> GridElement
elementAt grid (row, col) =
  let (element, _) = (grid !! row) !! col
   in (element, (row, col))

getElement :: GridElement -> Element
getElement (element, _) = element

parseRow :: [Char] -> Position -> GridRow
parseRow [] _ = []
parseRow (x : xs) (row, col)
  | x == '.' = (GridEmpty, (row, col)) : parseRow xs (row, col + 1)
  | isDigit x =
      let num = takeWhile isDigit (x : xs)
       in (GridNumber num, (row, col))
            : ( [(GridNumberTail (row, col), (row, col + c)) | c <- [1 .. (length num - 1)]]
                  ++ parseRow (drop (length num) (x : xs)) (row, col + length num)
              )
  | otherwise = (GridSymbol x, (row, col)) : parseRow xs (row, col + 1)

_parseGrid :: [String] -> Position -> [GridRow]
_parseGrid [] _ = []
_parseGrid (s : ss) (row, col) = parseRow s (row, col) : _parseGrid ss (row + 1, 0)

parseGrid :: [String] -> Grid
parseGrid s = _parseGrid s (0, 0)

validPos :: GridBounds -> Position -> Bool
validPos (gridMaxRow, gridMaxCol) (row, col) =
  (row >= 0 && row < gridMaxRow)
    && (col >= 0 && col < gridMaxCol)

positionsAround :: GridBounds -> GridElement -> [Position]
positionsAround gridBounds (GridNumber num, (row, col)) =
  filter
    (validPos gridBounds)
    ( [(r, c) | r <- [row - 1, row + 1], c <- [(col - 1) .. (col + length num)]]
        ++ [(row, col - 1), (row, col + length num)]
    )
positionsAround gridBounds (_, (row, col)) =
  filter
    (validPos gridBounds)
    ( [(r, c) | r <- [row - 1, row + 1], c <- [(col - 1) .. (col + 1)]]
        ++ [(row, col - 1), (row, col + 1)]
    )

isGridNumber :: GridElement -> Bool
isGridNumber (GridNumber _, _) = True
isGridNumber _ = False

isPart :: Element -> Bool
isPart (GridSymbol _) = True
isPart _ = False

partAdjacent :: Grid -> GridElement -> Bool
partAdjacent grid num =
  let gridBounds = (length grid, length (head grid))
   in any (isPart . getElement . elementAt grid) (positionsAround gridBounds num)

gridNumbers :: Grid -> [GridElement]
gridNumbers = filter isGridNumber . concat

gridNumbersWithParts :: Grid -> [GridElement]
gridNumbersWithParts grid = filter (partAdjacent grid) $ gridNumbers grid

answer :: String -> String
answer = show . sum . map (\(GridNumber num, _) -> read num :: Int) . gridNumbersWithParts . parseGrid . lines

data Day3Part1 = Day3Part1

instance Exercise Day3Part1 where
  resourceName _ = "day3p1.txt"
  prettyName _ = "Day 3: Part 1"
  solution _ = answer
