module AoC2023.Util.Parser
  ( Parser (parse),
    whitespace,
    literal,
    notLiteral,
    number,
    many,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isDigit, isSpace)

-- Simple parser implementation
newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

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

whitespace :: Parser String
whitespace = Parser $ \input ->
  case span isSpace input of
    ("", _) -> Nothing
    (_, rest) -> Just ("", rest)

literal :: String -> Parser String
literal lit = Parser $ \input ->
  if take (length lit) input == lit
    then Just (splitAt (length lit) input)
    else Nothing

notLiteral :: String -> Parser String
notLiteral lit = Parser $ \input ->
  if take (length lit) input == lit
    then Nothing
    else Just (splitAt (length lit) input)

number :: Parser Int
number =
  (\digits -> read digits :: Int)
    <$> Parser
      ( \input ->
          case span isDigit input of
            ("", _) -> Nothing
            (digits, rest) -> Just (digits, rest)
      )

many :: Parser a -> Parser [a]
many p = many' <|> pure []
  where
    many' = (:) <$> p <*> many p
