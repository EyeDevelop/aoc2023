{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module AoC2023.Day11.Part1
  ( Day11Part1 (Day11Part1),
  )
where

import AoC2023.Exercise (Exercise (..))
import Data.List (transpose)

expandRows :: [String] -> [String]
expandRows [] = []
expandRows (x : xs)
  | all ('.' ==) x = [x | _ <- [1..1_000_000]] ++ expandRows xs
  | otherwise = x : expandRows xs

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

expand :: [String] -> [String]
expand = transpose . expandRows . transpose . expandRows

galaxies :: [((Int, Int), Char)] -> [(Int, Int)]
galaxies = map fst . filter (('#' == ) . snd)

pairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pairs [] = []
pairs (x:xs) = map (x, ) xs ++ pairs xs

addCoords :: [[Char]] -> [((Int, Int), Char)]
addCoords lst = concatMap (\(rowInd, row) -> zipWith (\colInd col -> ((rowInd, colInd), col)) [0..] row ) $ zip [0..] lst

answer :: String -> String
answer = show . sum . map (uncurry distance) . pairs . galaxies . addCoords . expand . lines

data Day11Part1 = Day11Part1

instance Exercise Day11Part1 where
  resourceName _ = "day11p1.txt"
  prettyName _ = "Day 11: Part 1"
  solution _ = answer