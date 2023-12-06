module AoC2023.Day6.Part1
  ( Day6Part1 (Day6Part1),
    Time,
    Distance,
    Race (Race),
    waysToWin,
  )
where

import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (Parser (parse), literal, many, number, whitespace)

type Time = Int

type Distance = Int

newtype Race = Race (Time, Distance) deriving (Show)

raceLength :: Int -> Int -> Int
raceLength raceDuration holdTime = (raceDuration - holdTime) * holdTime

beatsRecord :: Race -> Int -> Bool
beatsRecord (Race (raceDuration, record)) holdTime = record < raceLength raceDuration holdTime

waysToWin :: Race -> Int
waysToWin (Race (maxTime, d)) = countWays (Race (maxTime, d)) [0 .. maxTime]
  where
    countWays :: Race -> [Int] -> Int
    countWays _ [] = 0
    countWays (Race (raceDuration, record)) (try : tries)
      | beatsRecord (Race (raceDuration, record)) try = 1 + countWays (Race (raceDuration, record)) tries
      | otherwise = countWays (Race (raceDuration, record)) tries

parseRace :: Parser [Race]
parseRace = (\t _ d -> zipWith (curry Race) t d) <$> parseTime <*> whitespace <*> parseDistance
  where
    parseTime :: Parser [Time]
    parseTime = (\_ _ n ns -> n : ns) <$> literal "Time:" <*> whitespace <*> number <*> many ((\_ n -> n) <$> whitespace <*> number)

    parseDistance :: Parser [Distance]
    parseDistance = (\_ _ n ns -> n : ns) <$> literal "Distance:" <*> whitespace <*> number <*> many ((\_ n -> n) <$> whitespace <*> number)

takeRaces :: Maybe ([Race], String) -> [Race]
takeRaces (Just (r, _)) = r
takeRaces _ = error "Could not parse race."

answer :: String -> String
answer = show . product . map waysToWin . takeRaces . parse parseRace

data Day6Part1 = Day6Part1

instance Exercise Day6Part1 where
  resourceName _ = "day6p1.txt"
  prettyName _ = "Day 6: Part 1"
  solution _ = answer
