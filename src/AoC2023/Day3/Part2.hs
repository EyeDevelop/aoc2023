module AoC2023.Day3.Part2
  ( Day3Part2 (Day3Part2),
  )
where

import AoC2023.Day3.Part1 (parseGrid, Grid, GridElement, Element (..), positionsAround, isGridNumber, elementAt)
import AoC2023.Exercise (Exercise (..))

isPossibleGear :: GridElement -> Bool
isPossibleGear (GridSymbol '*', _) = True
isPossibleGear _ = False

possibleGears :: Grid -> [GridElement]
possibleGears = filter isPossibleGear . concat

elementsAround :: Grid -> GridElement -> [Element]
elementsAround grid element =
    let gridBounds = (length grid, length (head grid))
    in map (elementAt grid) (positionsAround gridBounds element)

stripTailsAfterNumber :: [Element] -> [Element]
stripTailsAfterNumber [] = []
stripTailsAfterNumber ((GridNumber n):xs) = GridNumber n : stripTailsAfterNumber (dropWhile isGridNumberTail xs)
stripTailsAfterNumber (x:xs) = x : stripTailsAfterNumber xs

isGridNumberTail :: Element -> Bool
isGridNumberTail (GridNumberTail _) = True
isGridNumberTail _ = False

isGridNumberLike :: Element -> Bool
isGridNumberLike element = isGridNumberTail element || isGridNumber element

isGear :: Grid -> GridElement -> Bool
isGear grid element = length (filter isGridNumberLike (stripTailsAfterNumber (elementsAround grid element))) == 2

gridGears :: Grid -> [GridElement]
gridGears grid = filter (isGear grid) (possibleGears grid)

getNumber :: Element -> Int
getNumber (GridNumber n) = read n::Int
getNumber (GridNumberTail n) = read n::Int
getNumber _ = error "No number!"

numbersAroundGear :: Grid -> GridElement -> [Int]
numbersAroundGear grid element = map getNumber $ filter isGridNumberLike (stripTailsAfterNumber (elementsAround grid element))

answer :: String -> String
answer s =
    let grid = parseGrid (lines s)
    in show $ sum $ map (product . numbersAroundGear grid) (gridGears grid)

data Day3Part2 = Day3Part2

instance Exercise Day3Part2 where
  resourceName _ = "day3p2.txt"
  prettyName _ = "Day 3: Part 2"
  solution _ = answer
