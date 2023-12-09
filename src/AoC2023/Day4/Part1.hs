module AoC2023.Day4.Part1
  ( Day4Part1 (Day4Part1),
    Card (Card),
    parseCard,
  )
where

import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser

newtype Card = Card (Int, [Int], [Int]) deriving (Show)

parseCard :: Parser Card
parseCard = makeCard <$> literal "Card" <*> whitespace <*> number <*> literal ":" <*> whitespace <*> numbers <*> whitespace <*> literal "|" <*> whitespace <*> numbers
  where
    makeCard _ _ cardId _ _ winningNumbers _ _ _ nums = Card (cardId, winningNumbers, nums)

    numbers :: Parser [Int]
    numbers = (:) <$> number <*> many ((\_ n -> n) <$> whitespace <*> number)

getScore :: Card -> Int
getScore = getScoreWith 0 (const 1)
  where
    getScoreWith :: Int -> (Int -> Int) -> Card -> Int
    getScoreWith score _ (Card (_, _, [])) = score
    getScoreWith score fn (Card (card, winners, n : ns))
      | n `elem` winners = getScoreWith (fn score) (* 2) (Card (card, winners, ns))
      | otherwise = getScoreWith score fn (Card (card, winners, ns))

answer :: String -> String
answer = show . sum . map (getScore . takeJust . parse parseCard) . lines

data Day4Part1 = Day4Part1

instance Exercise Day4Part1 where
  resourceName _ = "day4p1.txt"
  prettyName _ = "Day 4: Part 1"
  solution _ = answer
