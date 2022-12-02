#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE LambdaCase #-}

import           Data.Function   ((&))
import           Data.List.Split (splitOn)

main :: IO ()
main = do
    txt <- readFile "input.txt"
    let result1
          = sum
          $ scoreLine'
          . parseLine1
          <$> lines txt
    let result2 = sum $ scoreLine' . fmap runStrategy . parseLine2 <$> lines txt

    print (result1, result2)

data Throw = Rock | Paper | Scissors
  deriving (Show)
data Goal = Win | Lose | Draw
  deriving (Show)

parseThrow :: Char -> Maybe Throw
parseThrow = \case
  'A' -> Just Rock
  'B' -> Just Paper
  'C' -> Just Scissors
  'X' -> Just Rock
  'Y' -> Just Paper
  'Z' -> Just Scissors
  _   -> Nothing


chooseThrow :: Throw -> Goal -> Throw
chooseThrow t r = case (t, r) of
  (Rock, Win)      -> Paper
  (Rock, Lose)     -> Scissors
  (Rock, Draw)     -> Rock
  (Paper, Win)     -> Scissors
  (Paper, Lose)    -> Rock
  (Paper, Draw)    -> Paper
  (Scissors, Win)  -> Rock
  (Scissors, Lose) -> Paper
  (Scissors, Draw) -> Scissors

parseGoal :: Char -> Maybe Goal
parseGoal = \case
  'X' -> Just Lose
  'Y' -> Just Draw
  'Z' -> Just Win
  _   -> Nothing

parseLine1 :: String -> Maybe (Throw, Throw)
parseLine1 = \case
  [a, ' ', b] -> do
    lhs <- parseThrow a
    rhs <- parseThrow b
    return (lhs, rhs)

  _ -> Nothing

parseLine2 :: String -> Maybe (Throw, Goal)
parseLine2 = \case
  [a, ' ', b] -> do
    lhs <- parseThrow a
    rhs <- parseGoal b
    return (lhs, rhs)

  _ -> Nothing

scoreLine' :: Maybe (Throw, Throw) -> Int
scoreLine' = maybe 0 scoreLine

scoreLine :: (Throw, Throw) -> Int
scoreLine = \case
  (Rock, Rock)         -> 3 + 1
  (Rock, Paper)        -> 6 + 2
  (Rock, Scissors)     -> 0 + 3

  (Paper, Rock)        -> 0 + 1
  (Paper, Paper)       -> 3 + 2
  (Paper, Scissors)    -> 6 + 3

  (Scissors, Rock)     -> 6 + 1
  (Scissors, Paper)    -> 0 + 2
  (Scissors, Scissors) -> 3 + 3

runStrategy :: (Throw, Goal) -> (Throw, Throw)
runStrategy (a, b) =  (a, chooseThrow a b)
