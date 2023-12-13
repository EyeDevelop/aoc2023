module AoC2023.Day3.Part2
  ( Day3Part2 (Day3Part2),
  )
where

import AoC2023.Day3.Part1 (Element (..), Grid, GridElement, Position, elementAt, isGridNumber, parseGrid, positionsAround)
import AoC2023.Exercise (Exercise (..))

isPossibleGear :: GridElement -> Bool
isPossibleGear (GridSymbol '*', _) = True
isPossibleGear _ = False

possibleGears :: Grid -> [GridElement]
possibleGears = filter isPossibleGear . concat

elementsAround :: Grid -> GridElement -> [GridElement]
elementsAround grid element =
  let gridBounds = (length grid, length (head grid))
   in map (elementAt grid) (positionsAround gridBounds element)

isTailOfNumber :: Position -> GridElement -> Bool
isTailOfNumber p (GridNumberTail p2, _) = p == p2
isTailOfNumber _ _ = False

stripTailsAfterNumber :: [GridElement] -> [GridElement]
stripTailsAfterNumber [] = []
stripTailsAfterNumber ((GridNumber n, pos) : xs) = (GridNumber n, pos) : stripTailsAfterNumber (dropWhile (isTailOfNumber pos) xs)
stripTailsAfterNumber ((GridNumberTail ptr, pos) : xs) = (GridNumberTail ptr, pos) : stripTailsAfterNumber (dropWhile (isTailOfNumber ptr) xs)
stripTailsAfterNumber (x : xs) = x : stripTailsAfterNumber xs

isGridNumberTail :: GridElement -> Bool
isGridNumberTail (GridNumberTail _, _) = True
isGridNumberTail _ = False

isGridNumberLike :: GridElement -> Bool
isGridNumberLike element = isGridNumberTail element || isGridNumber element

isGear :: Grid -> GridElement -> Bool
isGear grid element = length (filter isGridNumberLike (stripTailsAfterNumber (elementsAround grid element))) == 2

gridGears :: Grid -> [GridElement]
gridGears grid = filter (isGear grid) (possibleGears grid)

getNumber :: Grid -> GridElement -> Int
getNumber _ (GridNumber n, _) = read n :: Int
getNumber grid (GridNumberTail pos, _) = getNumber grid (elementAt grid pos)
getNumber _ _ = error "No number!"

numbersAroundGear :: Grid -> GridElement -> [Int]
numbersAroundGear grid element = map (getNumber grid) $ filter isGridNumberLike (stripTailsAfterNumber (elementsAround grid element))

answer :: String -> String
answer s =
  let grid = parseGrid (lines s)
   in show $ sum $ map (product . numbersAroundGear grid) $ gridGears grid

data Day3Part2 = Day3Part2

instance Exercise Day3Part2 where
  resourceName _ = "day3p1.txt"
  prettyName _ = "Day 3: Part 2"
  solution _ = answer
