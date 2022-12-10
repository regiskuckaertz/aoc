{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forM_, void, when)
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Data.Functor (($>))
import Data.List (transpose)
import Data.Maybe (catMaybes)
import GHC.Arr
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.String

data Crates = C StackId Stack deriving (Show)
type Crate = Char
type Stack = [Crate]
type StackId = Int
data Move = M Int StackId StackId deriving (Show)

main = parseFromFile instructions "input.txt" >>= \case
  Left err -> 
    print err
  Right (crates, moves) -> do
    print crates
    print moves
    let result = runST $ do
              s <- newSTArray (1, length crates) []
              forM_ crates $ \(C i c) -> writeSTArray s i c
              forM_ moves $ doMove s
              arr <- freezeSTArray s
              return $ head <$> elems arr
    print result

doMove arr (M n fro to)  = do
  fro' <- readSTArray arr fro
  let (xs, ys) = splitAt n fro'
  writeSTArray arr fro ys
  to' <- readSTArray arr to
  writeSTArray arr to (xs ++ to')

instructions :: Parser ([Crates], [Move]) 
instructions =
  (,) <$> (crates <* newline) <*> (moves <* eof)

crates :: Parser [Crates]
crates = do 
  stacks <- fmap catMaybes . transpose <$> many1 cratesH
  ids <- (\n -> [1..n]) <$> stackNs
  newline
  return $ zipWith C ids stacks

cratesH :: Parser [Maybe Crate]
cratesH = sepBy (try maybeCrate) (char ' ') <* newline

maybeCrate = Just <$> crate <|> Nothing <$ emptycreate

crate :: Parser Crate
crate = between (char '[') (char ']') upper

stackNs :: Parser Int
stackNs = execStateT go 0 
  where 
    go = do
      modify (+1)
      n <- get
      lift $ stackN n
      more <- lift $ char ' ' $> True <|> return False
      when more go

stackN :: Int -> Parser ()
stackN n = void $ space *> string (show n) *> space

moves :: Parser [Move]
moves = many1 movement

movement :: Parser Move
movement = do
  string "move"
  space
  qty <- int
  space
  string "from"
  space
  from <- int
  space
  string "to"
  space
  to <- int
  newline
  return $ M qty from to

emptycreate :: Parser ()
emptycreate = void $ space *> space *> space

int :: Parser Int
int = read <$> many1 digit