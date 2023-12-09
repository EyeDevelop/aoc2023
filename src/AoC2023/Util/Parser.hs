module AoC2023.Util.Parser
  ( Parser (parse),
    whitespace,
    word,
    newline,
    literal,
    notLiteral,
    digit,
    number,
    numberList,
    many,
    times,
    delimited,
    enclosed,
    parens,
    takeJust,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isAlphaNum, isControl, isDigit, isSpace)
import Data.List (isPrefixOf)

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
  case span (\c -> isSpace c && not (isControl c)) input of
    ("", _) -> Nothing
    (_, rest) -> Just ("", rest)

word :: Parser String
word = Parser $ \input ->
  case span isAlphaNum input of
    ("", _) -> Nothing
    (w, rest) -> Just (w, rest)

newline :: Parser String
newline = literal "\r\n" <|> literal "\n"

literal :: String -> Parser String
literal lit = Parser $ \input ->
  if lit `isPrefixOf` input
    then Just (lit, drop (length lit) input)
    else Nothing

notLiteral :: String -> Parser String
notLiteral lit = Parser $ \input ->
  if lit `isPrefixOf` input || null input
    then Nothing
    else Just (lit, drop (length lit) input)

digit :: Parser String
digit = Parser $ \input ->
  if not (null input) && isDigit (head input)
    then Just ([head input], drop 1 input)
    else Nothing

number :: Parser Int
number =
  ( (\a b -> read (a ++ foldr1 (++) b) :: Int)
      <$> literal "-"
      <*> many digit
  )
    <|> ((\c -> read (foldr1 (++) c) :: Int) <$> many digit)

numberList :: Parser [Int]
numberList = whitespace `delimited` number

many :: Parser a -> Parser [a]
many p = many' <|> pure []
  where
    many' = (:) <$> p <*> many p

times :: Int -> Parser a -> Parser [a]
0 `times` _ = pure []
count `times` p = (:) <$> p <*> times (count - 1) p

delimited :: Parser a -> Parser b -> Parser [b]
delimited a b = (:) <$> b <*> many ((\_ v -> v) <$> a <*> b)

enclosed :: Parser a -> Parser b -> Parser c -> Parser b
enclosed a b c = (\_ r _ -> r) <$> a <*> b <*> c

parens :: Parser b -> Parser b
parens b = enclosed (literal "(") b (literal ")")

takeJust :: Maybe (a, String) -> a
takeJust (Just (v, _)) = v
takeJust _ = error "Could not parse!"