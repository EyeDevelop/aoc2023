module AoC2023.Day2.Part1 (
    Day2Part1 (Day2Part1),
    Colour (..),
    ColourCount (..),
    GameSet,
    Game (..),
    parseGames,
) where

import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (Parser (parse), literal, number, whitespace, many)
import Control.Applicative (Alternative((<|>)))

import Prelude hiding (id)

-- Tokens
data Colour = Red | Green | Blue deriving (Eq, Show)
data ColourCount = ColourCount {
    colour :: !Colour,
    count :: !Int
} deriving (Eq, Show)
type GameSet = [ColourCount]
data Game = Game {
    gameId :: !Int,
    sets :: ![GameSet]
} deriving (Eq, Show)

-- Grammar
-- Color: "red" | "green" | "blue"
-- Number: [0-9]
-- ColorCount: Number Color | Number Color "," ColorCount
-- GameSet: ColorCount | ColorCount ";" GameSet
-- Game: Number GameSet

parseColor :: Parser Colour
parseColor = colourFrom <$> (literal "red" <|> literal "green" <|> literal "blue")
    where
        colourFrom "red" = Red
        colourFrom "green" = Green
        colourFrom "blue" = Blue
        colourFrom _ = error "No such colour!"
        

colourCount :: Parser ColourCount
colourCount = (\num _ col -> ColourCount { colour = col, count = num}) <$> number <*> whitespace <*> parseColor

gameSet :: Parser GameSet
gameSet = (:) <$> colourCount <*> many ((\_ _ c -> c) <$> literal "," <*> whitespace <*> colourCount)

game :: Parser Game
game = (\_ _ gid _ _ s ss -> Game { gameId = gid, sets = s:ss }) <$> literal "Game" <*> whitespace <*> number <*> literal ":" <*> whitespace <*> gameSet <*> many ((\_ _ g -> g) <$> literal ";" <*> whitespace <*> gameSet)

maxColour :: Colour -> Int
maxColour Red = 12
maxColour Green = 13
maxColour Blue = 14

gameSetValid :: GameSet -> Bool
gameSetValid [] = True
gameSetValid (c:cs)
  | count c > maxColour (colour c) = False
  | otherwise = gameSetValid cs

gameValid :: Game -> Bool
gameValid g = all gameSetValid (sets g)

takeGame :: Maybe (Game, String) -> Game
takeGame (Just (g, _)) = g
takeGame _ = error "Could not parse game"

validGames :: [Game] -> [Game]
validGames = filter gameValid

parseGames :: [String] -> [Game]
parseGames = map (takeGame . parse game)

answer :: String -> String
answer = show . sum . map gameId . validGames . parseGames . lines

data Day2Part1 = Day2Part1
instance Exercise Day2Part1 where
    resourceName _ = "day2p1.txt"
    prettyName _ = "Day 2: Part 1"
    solution _ = answer
