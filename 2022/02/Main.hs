{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (round)
import Data.Functor (($>))
import Data.Semigroup (Sum(..))
import Text.Parsec
import Text.Parsec.String

data Hand = R | P | S

type Round = (Hand, Hand)

score :: Round -> (Sum Int, Sum Int)
score (opponent, you) = (Sum score1, Sum score2)
  where 
    score1 = shape opponent + outcome you opponent
    score2 = shape you + outcome opponent you
    shape = \case
      R -> 1
      P -> 2
      S -> 3
    outcome S R = 6
    outcome P S = 6
    outcome R P = 6
    outcome R R = 3
    outcome P P = 3
    outcome S S = 3
    outcome _ _ = 0
  
lose R = S
lose P = R
lose S = P

draw = id

win R = P
win P = S
win S = R

handOpponent :: Parser Hand
handOpponent = 
  char 'A' $> R
  <|> char 'B' $> P
  <|> char 'C' $> S

handYou :: Hand -> Parser Hand
handYou hand = 
  char 'X' $> lose hand
  <|> char 'Y' $> draw hand
  <|> char 'Z' $> win hand

round :: Parser Round
round = do
  opp <- handOpponent
  space
  you <- handYou opp
  newline
  return (opp, you)

rock_paper_scissors :: Parser [Round]
rock_paper_scissors = many1 round <* eof

main =
  parseFromFile rock_paper_scissors "input.txt" >>= \case
    Left err -> 
      print err
    Right rounds -> 
      let (opponent, you) = foldMap score rounds
      in print (opponent, you)