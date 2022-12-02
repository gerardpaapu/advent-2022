#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

import           Data.Function   ((&))
import           Data.List       (sort)
import           Data.List.Split (splitOn)

main :: IO ()
main = do
  txt <- readFile "./input.txt"
  let elves = lines txt
                & splitOn [""]
                & fmap (read <$>)
                & fmap sum :: [Int]

  let part1 = maximum elves
  let part2 = sort elves & reverse & take 3 & sum
  print (part1, part2)
