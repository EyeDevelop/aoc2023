{-# LANGUAGE LambdaCase #-}
module AoC2023.Day2.Part1 (Day2Part1 (Day2Part1)) where

import Data.Char (isSpace, isDigit)
import Control.Applicative (Alternative (empty, (<|>)))

import AoC2023.Exercise (Exercise (..))

-- Tokens
data Color = Red | Green | Blue deriving (Eq, Show)
data ColorCount = ColorCount {
    color :: !Color,
    count :: !Int
} deriving (Eq, Show)
newtype GameSet = GameSet {
    counts:: [ColorCount]
} deriving (Eq, Show)
data Game = Game {
    gameId :: !Int,
    sets :: ![GameSet]
} deriving (Eq, Show)

-- Simple parser implementation
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }
instance Functor Parser where
    fmap func p = Parser $ \input ->
        case parse p input of
            Nothing -> Nothing
            Just (token, rest) -> Just (func token, rest)

instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    (<*>) parserA parserB = Parser $ \input ->
        case parse parserA input of
            Nothing -> Nothing
            Just (f, restA) -> case parse parserB restA of
                Nothing -> Nothing
                Just (val, restB) -> Just (f val, restB)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (<|>) parserA parserB = Parser $ \input ->
        case parse parserA input of
            Just res -> Just res
            Nothing -> parse parserB input

-- Grammar
-- Color: "red" | "green" | "blue"
-- Number: [0-9]
-- ColorCount: Number Color | Number Color "," ColorCount
-- GameSet: ColorCount | ColorCount ";" GameSet
-- Game: Number GameSet

whitespace :: Parser String
whitespace = Parser $ \input ->
    case span isSpace input of
        ("", _) -> Nothing
        (_, rest) -> Just ("", rest)

literal :: String -> Parser String
literal lit = Parser $ \input ->
    if take litLen input == lit
        then Just (lit, drop litLen input)
        else Nothing
    where litLen = length lit

number :: Parser Int
number = charsToInt <$> Parser (\input ->
    case span isDigit input of
        ("", _) -> Nothing
        (digits, rest) -> Just (digits, rest))
    where
        charsToInt digits = read digits::Int

many :: Parser a -> Parser [a]
many p = many' <|> pure []
    where
        many' = (:) <$> p <*> many p

colorFrom :: String -> Color
colorFrom "red" = Red
colorFrom "green" = Green
colorFrom "blue" = Blue
colorFrom _ = error "No such color"

parseColor :: Parser Color
parseColor = colorFrom <$> (literal "red" <|> literal "green" <|> literal "blue")

colorCount :: Parser ColorCount
colorCount = convertColorCount <$> number <*> whitespace <*> parseColor
    where
        convertColorCount num _ col = ColorCount { color = col, count = num }

colorCounts :: Parser [ColorCount]
colorCounts = (\c cs -> c:cs) <$> colorCount <*> many ((\_ _ c -> c) <$> (literal ",") <*> whitespace <*> colorCount)

gameSet :: Parser GameSet
gameSet = convertGameSet <$> colorCounts
    where
        convertGameSet cs = GameSet { counts = cs }

game :: Parser Game
game = convertGame <$> (literal "Game") <*> whitespace <*> number <*> (literal ":") <*> whitespace <*> gameSet <*> many ((\_ _ g -> g) <$> (literal ";") <*> whitespace <*> gameSet)
    where
        convertGame _ _ gid _ _ s ss = Game { gameId = gid, sets = s:ss}

answer :: String -> String
answer _ = show $ parse game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

data Day2Part1 = Day2Part1
instance Exercise Day2Part1 where
    resourceName _ = "day2p1.txt"
    prettyName _ = "Day 2: Part 1"
    solution _ = answer
