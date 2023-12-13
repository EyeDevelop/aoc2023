module AoC2023.Day2.Part2
  ( Day2Part2 (Day2Part2),
  )
where

import AoC2023.Day2.Part1 (Colour (..), ColourCount (colour, count), Game (..), GameSet, parseGames)
import AoC2023.Exercise (Exercise (..))

maxColourCount :: ColourCount -> (Int, Int, Int) -> (Int, Int, Int)
maxColourCount c (r, g, b) = case colour c of
  Red -> (max r (count c), g, b)
  Green -> (r, max g (count c), b)
  Blue -> (r, g, max b (count c))

maxColourCountsInSet :: GameSet -> (Int, Int, Int)
maxColourCountsInSet = foldr maxColourCount (0, 0, 0)

combineMaxCounts :: [(Int, Int, Int)] -> (Int, Int, Int)
combineMaxCounts [] = error "Empty list"
combineMaxCounts [x] = x
combineMaxCounts ((xr, xg, xb) : (yr, yg, yb) : xs) = combineMaxCounts ((max xr yr, max xg yg, max xb yb) : xs)

maxColourCounts :: Game -> (Int, Int, Int)
maxColourCounts = combineMaxCounts . map maxColourCountsInSet . sets

cubePower :: (Int, Int, Int) -> Int
cubePower (r, g, b) = r * g * b

answer :: String -> String
answer = show . sum . map (cubePower . maxColourCounts) . parseGames . lines

data Day2Part2 = Day2Part2

instance Exercise Day2Part2 where
  resourceName _ = "day2p1.txt"
  prettyName _ = "Day 2: Part 2"
  solution _ = answer
