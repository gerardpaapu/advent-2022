#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE LambdaCase #-}

-- stack --resolver lts-12.21 script
{-# LANGUAGE LambdaCase #-}
import           Data.Char       (digitToInt, isAsciiLower, isAsciiUpper, ord)
import           Data.Function   ((&))
import           Data.List       (intersect)
import           Data.List.Split (splitOn)

example :: String
example = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

main :: IO ()
main = do
  txt <- readFile "input.txt"
  -- let txt = example
  let part1 = txt & lines & fmap parsePair & filter oneContainsTheOther & length
  let part2 = txt & lines & fmap parsePair & filter overlaps & length
  print (part1, part2)

parseRange :: String -> (Int, Int)
parseRange s = case splitOn "-" s of
  [a, b] -> (read a, read b)
  _      -> error ("Fuck" ++ show s)

parsePair :: String -> ((Int, Int), (Int, Int))
parsePair c = case splitOn "," c of
  [a, b] -> (parseRange a, parseRange b)
  _      -> error "Invalid pair"

contains (a, b) (x, y) =
  a <= x && b >= y

oneContainsTheOther (a, b) = contains a b || contains b a

overlaps ((a, b), (x, y)) =
  -- :/ some of this feels redundant
  contains (a, b) x || contains (a, b) y || contains (x, y) a || contains (x, y ) b
  where
    contains (a, b) x = do
      let aa = min a b
      let bb = max a b
      aa <= x && x <= bb

