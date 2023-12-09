{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module AoC2023.Day7.Part1
  ( Day7Part1 (Day7Part1),
  )
where

import AoC2023.Exercise (Exercise (..))
import qualified Data.Ord as Ordering
import Data.List (sortOn, nub)
import AoC2023.Util.Parser (takeJust, Parser (parse), times, literal, digit, number, whitespace, many, newline)
import Control.Applicative (Alternative((<|>)))

data Card
  = Ace
  | King
  | Queen
  | Jack
  | Number Int
  deriving (Eq, Show)

instance Ord Card where
    _ <= Ace = True
    Ace <= _ = False

    _ <= King = True
    King <= _ = False

    _ <= Queen = True
    Queen <= _ = False

    _ <= Jack = True
    Jack <= _ = False

    (Number a) <= (Number b) = a <= b

data CardHandResult
  = FiveOfAKind Card
  | FourOfAKind Card
  | FullHouse (Card, Card)
  | ThreeOfAKind Card
  | TwoPair (Card, Card)
  | OnePair Card
  | HighCard Card
  deriving (Show)

instance Eq CardHandResult where
    FiveOfAKind _ == FiveOfAKind _ = True
    FourOfAKind _ == FourOfAKind _ = True
    FullHouse _ == FullHouse _ = True
    ThreeOfAKind _ == ThreeOfAKind _ = True
    TwoPair _ == TwoPair _ = True
    OnePair _ == OnePair _ = True
    HighCard _ == HighCard _ = True
    _ == _ = False

instance Ord CardHandResult where
        FiveOfAKind _ <= FiveOfAKind _ = False
        _ <= FiveOfAKind _ = True
        FiveOfAKind _ <= _ = False

        FourOfAKind _ <= FourOfAKind _ = False
        _ <= FourOfAKind _ = True
        FourOfAKind _ <= _ = False

        FullHouse _ <= FullHouse _ = False
        _ <= FullHouse _ = True
        FullHouse _ <= _ = False

        ThreeOfAKind _ <= ThreeOfAKind _ = False
        _ <= ThreeOfAKind _ = True
        ThreeOfAKind _ <= _ = False

        TwoPair _ <= TwoPair _ = False
        _ <= TwoPair _ = True
        TwoPair _ <= _ = False

        OnePair _ <= OnePair _ = False
        _ <= OnePair _ = True
        OnePair _ <= _ = False

        HighCard _ <= HighCard _ = False

newtype CardHand = CardHand (Card, Card, Card, Card, Card) deriving (Eq, Show)

instance Ord CardHand where
    CardHand a <= CardHand b = (handResult (CardHand a), a) <= (handResult (CardHand b), b)

handResult :: CardHand -> CardHandResult
handResult (CardHand (a, b, c, d, e)) =
    let cards = [a, b, c, d, e]
    in case nub (sortOn (Ordering.Down . snd) [(x, (length . filter (x==)) cards) | x <- cards]) of
        [(x, 5)] -> FiveOfAKind x
        [(x, 4), _] -> FourOfAKind x
        [(x, 3), (y, 2)] -> FullHouse (x, y)
        (x, 3):_ -> ThreeOfAKind x
        (x, 2):(y, 2):_ -> TwoPair (x, y)
        (x, 2):_ -> OnePair x
        _ -> HighCard (maximum cards)

parseHand :: Parser CardHand
parseHand = (\[a, b, c, d, e] -> CardHand (a, b, c, d, e)) <$> 5 `times` parseCard
  where
    cardFromStr :: String -> Card
    cardFromStr "A" = Ace
    cardFromStr "K" = King
    cardFromStr "Q" = Queen
    cardFromStr "J" = Jack
    cardFromStr "T" = Number 10
    cardFromStr n = Number (read n::Int)

    parseCard :: Parser Card
    parseCard = cardFromStr <$> (literal "A" <|> literal "K" <|> literal "Q" <|> literal "J" <|> literal "T" <|> digit)

parseHands :: Parser [(CardHand, Int)]
parseHands = many $ (\h _ n _ -> (h, n)) <$> parseHand <*> whitespace <*> number <*> (newline <|> pure "")

-- Assumes a sorted list!
scoreHands :: [(CardHand, Int)] -> [Int]
scoreHands = scoreHandsWithRank 1
  where
    scoreHandsWithRank :: Int -> [(CardHand, Int)] -> [Int]
    scoreHandsWithRank _ [] = []
    scoreHandsWithRank rank ((_, bet):hands) = rank * bet : scoreHandsWithRank (rank + 1) hands

answer :: String -> String
answer = show . sum . scoreHands . sortHands . takeJust . parse parseHands
  where
    sortHands :: [(CardHand, Int)] -> [(CardHand, Int)]
    sortHands = sortOn fst

data Day7Part1 = Day7Part1

instance Exercise Day7Part1 where
  resourceName _ = "day7p1.txt"
  prettyName _ = "Day 7: Part 1"
  solution _ = answer
