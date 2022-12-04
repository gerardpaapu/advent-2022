#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

import           Data.Char       (digitToInt, isAsciiLower, isAsciiUpper, ord)
import           Data.Function   ((&))
import           Data.List       (intersect)
import           Data.List.Split (splitOn)

example :: String
example = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

main :: IO ()
main = do
  txt <- readFile "input.txt"
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

contains :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Bool
contains (a, b) (x, y) =
  a <= x && b >= y

oneContainsTheOther :: (Ord a1, Ord a2) => ((a1, a2), (a1, a2)) -> Bool
oneContainsTheOther (a, b) = contains a b || contains b a

overlaps :: Ord a => ((a, a), (a, a)) -> Bool
overlaps ((a, b), (x, y)) =
  -- :/ some of this feels redundant
  contains (a, b) x || contains (a, b) y || contains (x, y) a || contains (x, y ) b
  where
    contains (a, b) x = do
      let aa = min a b
      let bb = max a b
      aa <= x && x <= bb

