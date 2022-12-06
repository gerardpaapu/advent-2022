#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

import           Data.Function   ((&))
import           Data.List       (intersect)
import           Data.List.Split (splitOn)
import           Text.Read       (readMaybe)


example :: String
example = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

main :: IO ()
main = do
  txt <- readFile "input.txt"
  pairs <- txt & lines & traverse parsePair
  let part1 = pairs & filter oneContainsTheOther & length
  let part2 = pairs & filter overlaps & length

  print (part1, part2)

parseRange :: MonadFail m => String -> m (Int, Int)
parseRange s = case splitOn "-" s of
  [a, b] -> do
    lhs <- readFail "shit" a
    rhs <- readFail "shit" a
    return (lhs, rhs)
  _else      -> fail ("Fuck" ++ show s)

readFail e a = readMaybe a & maybe (fail e) return

parsePair c = case splitOn "," c of
  [a, b] -> do
    lhs <- parseRange a
    rhs <- parseRange b
    return (lhs, rhs)
  _else   -> fail "Invalid pair"

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

