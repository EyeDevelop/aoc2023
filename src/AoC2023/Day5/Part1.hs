module AoC2023.Day5.Part1
  ( Day5Part1 (Day5Part1),
  )
where

import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (Parser (parse), delimited, literal, many, newline, notLiteral, number, numberList, times, whitespace, takeJust)

newtype Location = Location Int deriving (Show)

data Humidity
  = UnmappedHumidity Int
  | MappedHumidity Int Location
  deriving (Show)

data Temperature
  = UnmappedTemperature Int
  | MappedTemperature Int Humidity
  deriving (Show)

data Light
  = UnmappedLight Int
  | MappedLight Int Temperature
  deriving (Show)

data Water
  = UnmappedWater Int
  | MappedWater Int Light
  deriving (Show)

data Fertilizer
  = UnmappedFertilizer Int
  | MappedFertilizer Int Water
  deriving (Show)

data Seed
  = UnmappedSeed Int
  | MappedSeed Int Fertilizer
  deriving (Show)

newtype AlmanacMap
  = AlmanacMap [(Int, Int, Int)]
  deriving (Show)

parseAlmanac :: Parser ([Seed], [AlmanacMap])
parseAlmanac =
  (\s _ maps -> (s, maps))
    <$> parseSeeds
    <*> 2 `times` newline
    <*> many parseMap
  where
    parseSeeds :: Parser [Seed]
    parseSeeds = (\_ _ n -> map UnmappedSeed n) <$> literal "seeds:" <*> whitespace <*> numberList

    almanacMapEntry :: Parser (Int, Int, Int)
    almanacMapEntry = (\[d, s, r] -> (d, s, r)) <$> numberList

    parseMap :: Parser AlmanacMap
    parseMap = (\_ _ _ _ maps -> AlmanacMap maps) <$> many (notLiteral " ") <*> whitespace <*> literal "map:" <*> newline <*> newline `delimited` almanacMapEntry

answer :: String -> String
answer = show . takeJust . parse parseAlmanac

data Day5Part1 = Day5Part1

instance Exercise Day5Part1 where
  resourceName _ = "day5p1.txt"
  prettyName _ = "Day 5: Part 1"
  solution _ = answer
