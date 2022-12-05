{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.String

type Pair = (Range, Range)
type Range = (Int, Int)

main = parseFromFile pairs "input.txt" >>= \case
  Left err -> 
    print err
  Right ps -> 
    print $ length $ filter overlap ps

pairs :: Parser [Pair]
pairs = many1 pair <* eof

pair :: Parser Pair
pair = do
  r1 <- range
  char ','
  r2 <- range
  newline
  return (r1, r2)

range :: Parser Range
range = do
  a <- many1 digit
  char '-'
  b <- many1 digit
  return (read a, read b)

overlap :: Pair -> Bool
overlap ((a, b), (c, d)) =
  (a <= c && c <= b) || (c <= a && a <= d)