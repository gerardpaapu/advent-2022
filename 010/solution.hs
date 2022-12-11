#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text hiding (take)
import           Data.List            (intercalate, nub, scanl')
import           Data.Text.IO         (readFile)
import           Prelude              hiding (Either (..), readFile)

main :: IO ()
main = do
  txt <- readFile "input.txt"
  example <- readFile "example.txt"
  prg <- readProg txt
  let states = run 1 prg
  let strengths = signalStrength states
  let part1 = sum  [strengths !! 19, strengths !! 59, strengths !! 99, strengths !! 139, strengths !! 179, strengths !! 219]
  print part1

  let p = toRows 40 $ zipWith (\x pixel -> if pixel == x || pixel == x + 1 || pixel == x - 1 then '#' else '.') states (cycle [0..39])
  putStrLn $ intercalate "\n" p -- give them each their cycle number

type VM = (Int, Int)

data Instruction = Noop | AddX !Int
  deriving Show


toRows _ []     = []
toRows width ls = take width ls : toRows width (drop width ls)

readProg = either fail return . parseOnly ((instruction `sepBy` "\n") <* endOfInput)

signalStrength :: [Int] -> [Int]
signalStrength = zipWith (*) [1..]

runI :: Int -> Instruction -> ([Int], Int)
runI total = \case
  Noop   -> ([total], total)
  AddX n -> ([total, total], total + n)

-- run :: Int -> [Instruction] -> [Int]
run total (ix:ixs) = do
  let (cycles, end) = runI total ix
  cycles ++ run end ixs
run total []       = []

instruction = Noop <$ "noop" <|> AddX <$> ("addx " *> signed decimal)

