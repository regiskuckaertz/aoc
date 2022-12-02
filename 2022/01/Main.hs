module Main where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Text.Parsec
import Text.Parsec.String

type Elf = [Calories]
type Calories = Int

count_calories :: Elf -> Calories
count_calories = sum

most_calories :: [Calories] -> Calories
most_calories = maximum

exercise :: [Elf] -> Calories
exercise = most_calories . map count_calories

calories :: Parser Calories
calories = read <$> many1 digit <* newline

elf :: Parser Elf
elf = many1 calories

elves :: Parser [Elf]
elves = sepBy1 elf newline <* eof

main = do
  parseFromFile elves "input.txt" >>= \result ->
    case result of
      Left err -> print err
      Right elves -> print (exercise elves)