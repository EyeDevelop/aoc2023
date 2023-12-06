module AoC2023.Day6.Part2
  ( Day6Part2 (Day6Part2),
  )
where

import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (Parser (parse), literal, whitespace, number, many)
import AoC2023.Day6.Part1 (Race (Race), Time, Distance, waysToWin)

parseRace :: Parser Race
parseRace = (\t _ d -> Race (t, d)) <$> parseTime <*> whitespace <*> parseDistance
  where
    parseTime :: Parser Time
    parseTime = (\_ _ n ns -> read (show n ++ concat ns)::Int) <$> literal "Time:" <*> whitespace <*> number <*> many ((\_ n -> show n) <$> whitespace <*> number)

    parseDistance :: Parser Distance
    parseDistance = (\_ _ n ns -> read (show n ++ concat ns)::Int) <$> literal "Distance:" <*> whitespace <*> number <*> many ((\_ n -> show n) <$> whitespace <*> number)

takeRace :: Maybe (Race, String) -> Race
takeRace (Just (r, _)) = r
takeRace _ = error "Could not parse race."

answer :: String -> String
answer = show . waysToWin . takeRace . parse parseRace

data Day6Part2 = Day6Part2

instance Exercise Day6Part2 where
  resourceName _ = "day6p1.txt"
  prettyName _ = "Day 6: Part 1"
  solution _ = answer
