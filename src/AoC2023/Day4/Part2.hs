{-# LANGUAGE TupleSections #-}
module AoC2023.Day4.Part2 (
    Day4Part2 (Day4Part2)
) where

import AoC2023.Exercise (Exercise (..))
import AoC2023.Day4.Part1 (Card (Card), parseCard)

import AoC2023.Util.Parser (Parser(parse), takeJust)

matches :: Card -> Int
matches (Card (_, _, [])) = 0
matches (Card (card, winners, n:ns))
  | n `elem` winners = 1 + matches (Card (card, winners, ns))
  | otherwise = matches (Card (card, winners, ns))

playScratchCards :: [Card] -> Int
playScratchCards cards = sum $ map snd $ playRound (map (, 1) cards)
  where
    playRound :: [(Card, Int)] -> [(Card, Int)]
    playRound [] = []
    playRound ((card, multiplier):xs) =
        let matchCount = matches card
            roundResult = map (\(c, m) -> (c, m + multiplier)) (take matchCount xs) ++ drop matchCount xs
        in (card, multiplier) : playRound roundResult

-- [Card, multiplier]
-- 1: 4 matches   | [(1, 1), (2, 1), (3, 1), (4, 1), (5, 1)] --> [(1, 1), (2, 2), (3, 2), (4, 2), (5, 2)]
-- 2: 2 matches   | [(1, 1), (2, 2), (3, 2), (4, 2), (5, 2)] --> [(1, 1), (2, 2), (3, 2 + 1 * 2), (4, 2 + 1 * 2), (5, 2)]
-- 3: 2 matches   | [(1, 1), (2, 2), (3, 4), (4, 4), (5, 2)] --> [(1, 1), (2, 2), (3, 4), (4, 4 + 1 * 4), (5, 2 + 1 * 4)]
-- 4: 1 matches   | [(1, 1), (2, 2), (3, 4), (4, 8), (5, 6)] --> [(1, 1), (2, 2), (3, 4), (4, 8), (5, 6 + 1 * 8)]
-- 5: 0 matches   | [(1, 1), (2, 2), (3, 4), (4, 8), (5, 14)]

answer :: String -> String
answer = show . playScratchCards . map (takeJust . parse parseCard) . lines

data Day4Part2 = Day4Part2

instance Exercise Day4Part2 where
  resourceName _ = "day4p1.txt"
  prettyName _ = "Day 4: Part 2"
  solution _ = answer
