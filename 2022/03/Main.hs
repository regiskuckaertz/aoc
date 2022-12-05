{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (ord, toLower)
import Data.List (sort)
import Data.Semigroup (Sum(..))
import Text.Parsec
import Text.Parsec.String

main = parseFromFile (many1 ((,,) <$> rucksack <*> rucksack <*> rucksack)) "input.txt" >>= \case
  Left err -> 
    print err
  Right rucksacks -> 
    let result = foldMap (priority . findItem) rucksacks
    in print result

priority x | x >= 'a' && x <= 'z' = Sum (ord x - ord 'a' + 1)
           | otherwise = (26 +) <$> priority (toLower x)

findItem (cs, ds, es) = go cs' ds' es'
  where
    cs' = sort cs
    ds' = sort ds
    es' = sort es
    go (c : cs) (d : ds) (e: es) | c == d && c == e = c
                                 | c < d = go cs (d : ds) (e : es)
                                 | c < e = go cs (d : ds) (e : es)
                                 | d < e = go (c : cs) ds (e : es)
                                 | otherwise = go (c : cs) (d : ds) es

rucksack = many1 item <* newline

item = upper <|> lower