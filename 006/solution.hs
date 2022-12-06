#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

import           Data.List (nub)

main :: IO ()
main = do
  txt <- readFile "input.txt"
  let prefix = takeWhile hasDuplicates (quads txt)
  let part1 = length prefix + 4
  let prefix = takeWhile (\x -> nub x /= x)  $ quadteens txt
  let part2 = length prefix + 14
  print (part1, part2)

quads :: [a] -> [(a, a, a, a)]
quads (a:b:c:d:xs) = (a, b, c, d) : quads (b:c:d:xs)
quads xs           = []

quadteens :: [a] -> [[a]]
quadteens xs = take 14 xs : quadteens (tail xs)

hasDuplicates x = not (distinct x)

distinct (a, b, c, d) =
    -- fuckin... elegant bro
    a /= b && a /= c && a /= d && b /= c && b /= d && c /= d
