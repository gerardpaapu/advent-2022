#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE LambdaCase #-}

import           Data.Char     (isAsciiLower, isAsciiUpper, ord)
import           Data.Function ((&))
import           Data.List     (intersect)


main :: IO ()
main = do
  txt <- readFile "input.txt"
  let part1 = lines txt
              & fmap (toPriority . head . uncurry intersect . splitRucksack)
              & sum
  let part2 = lines txt
              & triplets
              & fmap (toPriority . head . common)
              & sum
  print (part1, part2)

triplets :: [c] -> [(c, c, c)]
triplets (a : b : c : xs) = (a, b, c) : triplets xs
triplets []               = []
triplets _                = []

common :: Eq a => ([a], [a], [a]) -> [a]
common (a, b, c) = a `intersect` b `intersect` c

toPriority :: Char -> Int
toPriority c
  | isAsciiLower c = ord c - 96
  | isAsciiUpper c =  ord c - 38
  | otherwise = 0

splitRucksack :: [a] -> ([a], [a])
splitRucksack l = do
  let len = length l `div` 2
  let lhs = take len l
  let rhs = drop len l
  (lhs, rhs)
