{-# LANGUAGE NumericUnderscores #-}

module AoC2023.Day11.Part2
  ( Day11Part2 (Day11Part2),
  )
where

import AoC2023.Day11.Part1 (addCoords, galaxies, pairs)
import AoC2023.Exercise (Exercise (..))
import Data.List (transpose)

emptyBetween :: [[Char]] -> Int -> Int -> Int
emptyBetween space ind1 ind2 =
  (length . filter (all ('.' ==))) $
    take (max ind1 ind2 - min ind1 ind2 - 1) $
      drop (min ind1 ind2 + 1) space

distanceWith :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int
distanceWith space (x1, y1) (x2, y2) =
  let verticalExpansion = emptyBetween space x1 x2
      horizontalExpansion = emptyBetween (transpose space) y1 y2
   in abs (x2 - x1) + abs (y2 - y1) + (verticalExpansion + horizontalExpansion) * 999_999

answer :: String -> String
answer inp =
  let space = lines inp
   in (show . sum . map (uncurry (distanceWith space)) . pairs . galaxies . addCoords) space

data Day11Part2 = Day11Part2

instance Exercise Day11Part2 where
  resourceName _ = "day11p1.txt"
  prettyName _ = "Day 11: Part 2"
  solution _ = answer