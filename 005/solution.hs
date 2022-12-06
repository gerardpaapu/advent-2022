#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

{-# LANGUAGE OverloadedStrings #-}


import           Control.Applicative  ((<|>))
import           Control.Lens         (ix, over, (^?))
import           Control.Lens.Getter
import           Data.Attoparsec.Text
import           Data.Foldable        (foldl')
import           Data.Function        ((&))
import           Data.List            (take, transpose)
import           Data.Maybe           (catMaybes)
import           Data.Text.IO         as Text

main :: IO ()
main = do
  txt <- Text.readFile "input.txt"
  (g, steps) <- parseOnly data_ txt & either fail return
  let stacks = transpose g & fmap catMaybes
  let part1 = head <$> runAll stacks steps
  let part2 = head <$> runAll2 stacks steps
  print (part1, part2)


runAll :: (Foldable f) => [[a]] -> f (Int, Int, Int) -> [[a]]
runAll = foldl' run

runAll2 :: (Foldable f) => [[a]] -> f (Int, Int, Int) -> [[a]]
runAll2 = foldl' run2

run stacks (a, b, c)  =
  case a of
    0 -> stacks
    _ -> do
      let h = stacks !! (b - 1) & head
      let stacks1 = over (ix (b - 1)) tail stacks
      let stacks2 = over (ix (c - 1)) (h :) stacks1
      run stacks2 (a - 1, b, c)


run2 :: [[a]] -> (Int, Int, Int) -> [[a]]
run2 stacks (a, b, c) = do
  let h = stacks !! (b - 1) & Data.List.take a
  let stacks1 = over (ix (b - 1)) (drop a) stacks
  let stacks2 = over (ix (c - 1)) (h ++) stacks1
  stacks2


box = do
  "["
  l <- letter
  "]"
  return $ Just l

gap = do
  "   "
  return Nothing

line =  (box <|> gap) `sepBy1'` " "

grid = (line `sepBy1'` "\n") <* "\n"

move = do
  "move "
  a <- decimal
  " from "
  b <- decimal
  " to "
  c <-decimal
  return ((a, b, c) :: (Int, Int, Int))

numMarker = do
  " "
  decimal
  " "

numberRow = numMarker `sepBy1'` " " <* "\n\n"

data_ = do
  grid' <- grid
  numberRow
  b <- move `sepBy'` "\n"
  return (grid', b)

