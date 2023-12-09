module AoC2023.Day8.Part1
  ( Day8Part1 (Day8Part1),
    Direction (..),
    Node (..),
    parseDirections,
    findNode,
    takeDirection,
    walk,
  )
where

import AoC2023.Exercise (Exercise (..))
import AoC2023.Util.Parser (Parser (parse), delimited, literal, many, newline, parens, takeJust, whitespace, word)
import Control.Applicative (Alternative ((<|>)))
import Prelude hiding (Left, Right)

data Direction = Left | Right deriving (Show)

data Node = Node String String String deriving (Show)

parseDirections :: Parser ([Direction], [Node])
parseDirections = (\a _ b _ -> (a, b)) <$> parseInstruction <*> newline <*> parseNodes <*> newline
  where
    getInstruction :: String -> Direction
    getInstruction "R" = Right
    getInstruction "L" = Left
    getInstruction _ = error "Not a valid instruction!"

    parseInstruction :: Parser [Direction]
    parseInstruction = const <$> many (getInstruction <$> (literal "R" <|> literal "L")) <*> newline

    parseNode :: Parser Node
    parseNode =
      (\name _ _ _ [leftName, rightName] -> Node name leftName rightName)
        <$> word
        <*> whitespace
        <*> literal "="
        <*> whitespace
        <*> parens (((++) <$> literal "," <*> whitespace) `delimited` word)

    parseNodes :: Parser [Node]
    parseNodes = newline `delimited` parseNode

findNode :: String -> [Node] -> Node
findNode _ [] = error "No node in list!"
findNode name ((Node nodeName leftName rightName) : ns)
  | name == nodeName = Node nodeName leftName rightName
  | otherwise = findNode name ns

takeDirection :: [Node] -> Direction -> Node -> Node
takeDirection nodes Left (Node _ leftName _) = findNode leftName nodes
takeDirection nodes Right (Node _ _ rightName) = findNode rightName nodes

walk :: ([Direction], [Node]) -> Int
walk (directions, nodes) = walkWith 0 (findNode "AAA" nodes)
  where
    nextDirection :: Int -> Direction
    nextDirection step = directions !! (step `mod` length directions)

    walkWith :: Int -> Node -> Int
    walkWith count (Node "ZZZ" _ _) = count
    walkWith count currentNode = walkWith (count + 1) (takeDirection nodes (nextDirection count) currentNode)

answer :: String -> String
answer = show . walk . takeJust . parse parseDirections

data Day8Part1 = Day8Part1

instance Exercise Day8Part1 where
  resourceName _ = "day8p1.txt"
  prettyName _ = "Day 8: Part 1"
  solution _ = answer
