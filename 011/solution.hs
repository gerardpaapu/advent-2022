#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text hiding (take)
import           Data.List            (intercalate, nub, scanl')
import           Data.Text.IO         (readFile)
import           Prelude              hiding (Either (..), readFile)

main = do
  txt <- readFile "input.txt"
  let example = "Monkey 0:\n\
                \  Starting items: 64, 89, 65, 95\n\
                \  Operation: new = old * 7\n\
                \  Test: divisible by 3\n\
                \    If true: throw to monkey 4\n\
                \    If false: throw to monkey 1"
  prg <- readProg txt
  print prg

data Op = Plus | Mul
  deriving Show

data Rhs = Const !Int | Old
  deriving Show

readProg = either fail return . parseOnly (monkeyP `sepBy` "\n\n" <* endOfInput)

monkeyP :: Parser (Int, [Int], Op, Rhs, Int, Int)
monkeyP = do
  id      <- "Monkey " *> decimal <* ":\n"
  items   <- "  Starting items: " *> decimal `sepBy` ", " <* "\n"
  (op, k) <- "  Operation: new = old " *> update <* "\n"
  divBy   <- "  Test: divisible by " *> decimal <* "\n"
  ifTrue  <- "    If true: throw to monkey " *> decimal <* "\n"
  ifFalse <- "    If false: throw to monkey " *> decimal
  return (id, items, op, k, ifTrue, ifFalse)

  where
    update = do
      operation <- Plus <$ "+" <|> Mul <$ "*"
      " "
      rhs <- Const <$> decimal <|> Old <$ "old"
      return (operation, rhs)
