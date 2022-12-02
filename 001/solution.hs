#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

import           Data.Function   ((&))
import           Data.List.Split (splitOn)

main :: IO ()
main = do
  txt <- readFile "./input.txt"
  let result = lines txt
                & splitOn [""]
                & fmap (read <$>)
                & fmap sum
                & maximum :: Int
  print result
