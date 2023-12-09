module AoC2023.Day7.Part2
  ( Day7Part2 (Day7Part2),
  )
where

import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (Parser (parse), digit, literal, many, newline, number, takeJust, times, whitespace)
import Control.Applicative (Alternative ((<|>)))
import Data.List (nub, sortOn)
import qualified Data.Ord as Ordering

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
  (Number a) <= (Number b) = a <= b
  _ <= Number _ = True
  Number _ <= _ = False
  Jack <= _ = True

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
      jacks = (length . filter (Jack ==)) cards
   in calculateResult ((addJacks jacks . nub) (sortOn Ordering.Down [((length . filter (x ==) . filter (Jack /=)) cards, x) | x <- cards]))
  where
    calculateResult :: [(Int, Card)] -> CardHandResult
    calculateResult ((5, x) : _) = FiveOfAKind x
    calculateResult ((4, x) : _) = FourOfAKind x
    calculateResult ((3, x) : (2, y) : _) = FullHouse (x, y)
    calculateResult ((3, x) : _) = ThreeOfAKind x
    calculateResult ((2, x) : (2, y) : _) = TwoPair (x, y)
    calculateResult ((2, x) : _) = OnePair x
    calculateResult cards = HighCard (maximum (map snd cards))

    addJacks :: Int -> [(Int, Card)] -> [(Int, Card)]
    addJacks _ [] = []
    addJacks jacks ((occurrence, card) : cs) = (occurrence + jacks, card) : cs

parseHand :: Parser CardHand
parseHand = (\[a, b, c, d, e] -> CardHand (a, b, c, d, e)) <$> 5 `times` parseCard
  where
    cardFromStr :: String -> Card
    cardFromStr "A" = Ace
    cardFromStr "K" = King
    cardFromStr "Q" = Queen
    cardFromStr "J" = Jack
    cardFromStr "T" = Number 10
    cardFromStr n = Number (read n :: Int)

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
    scoreHandsWithRank rank ((_, bet) : hands) = rank * bet : scoreHandsWithRank (rank + 1) hands

answer :: String -> String
answer = show . sum . scoreHands . sortHands . takeJust . parse parseHands
  where
    sortHands :: [(CardHand, Int)] -> [(CardHand, Int)]
    sortHands = sortOn fst

data Day7Part2 = Day7Part2

instance Exercise Day7Part2 where
  resourceName _ = "day7p1.txt"
  prettyName _ = "Day 7: Part 2"
  solution _ = answer
