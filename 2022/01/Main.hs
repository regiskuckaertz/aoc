module Main where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Text.Parsec
import Text.Parsec.String

type Elf = [Calories]
type Calories = Int

count_calories :: Elf -> Calories
count_calories = sum

most_calories :: [Calories] -> (Calories, Calories, Calories)
most_calories = foldr merge (0, 0, 0)
  where merge x (a, b, c) | x > a = (x, a, b)
                          | x > b = (a, x, b)
                          | x > c = (a, b, x)
                          | otherwise = (a, b, c)

exercise :: [Elf] -> (Calories, Calories, Calories)
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
      Right elves -> 
        let (x, y, z) = exercise elves
        in do
          print (x, y, z)
          print (x + y + z)