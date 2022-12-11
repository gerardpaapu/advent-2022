#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}


import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text hiding (take)
import           Data.List            (nub, scanl')
import           Data.Text.IO         (readFile)
import           Prelude              hiding (Either (..), readFile)

main = do
  txt <- readFile "input.txt"
  prog <- either fail return $ parseOnly program txt

  let part1 = scanl' (flip doStep1) ((0, 0), (0, 0)) (compile prog)
  print $ length $ nub $ snd <$> part1

  prog2 <- either fail return $ parseOnly program txt
  let part2 = scanl' (flip doStep) (replicate 10 (0, 0)) (compile prog2)
  let part2' = settle $ last part2
  print $ length $ nub $ (!! 9) <$> (part2 ++ part2')

-- strictly speaking attoparsec is probably overkill for this one
-- but I'm just in the habit now

data Direction = Left | Right | Up | Down
  deriving Show

type Step = (Direction, Int)

type Rope1 = ((Int, Int), (Int, Int))
type Rope2 = [(Int, Int)]

-- we've moved our head, but the other parts may still be catching up
settle :: [(Int, Int)] -> [[(Int, Int)]]
settle rope = if next /= rope then
  next : settle next
else
  []
  where
    next = followAll rope

compile :: [Step] -> [(Int, Int)]
compile = concatMap compileStep

compileStep step = case step of
  (Left, n)  -> replicate n (-1, 0)
  (Right, n) -> replicate n (1, 0)
  (Up, n)    -> replicate n (0, -1)
  (Down, n)  -> replicate n (0, 1)

diff :: (Int, Int) -> (Int, Int) -> (Int, Int)
diff (hx, hy) (tx, ty) = (hx - tx, hy - ty)

follow' :: Rope1 -> Rope1
follow' (r, (x, y)) = (r, follow r (x, y))

follow r (x, y) = case diff r (x, y) of
  -- if they're 2 away in a cardinal direction
  -- follow them
  (0, 2)                          -> (x, y + 1)
  (0, -2)                         -> (x, y - 1)
  (2, 0)                          -> (x + 1, y)
  (-2, 0)                         -> (x - 1, y)
  -- otherwise if they're touching
  (dx, dy) | abs dx + abs dy <= 2 -> (x, y)
  -- they're not touching so make a diagonal move
  (dx, dy) | dx > 0 && dy > 0     -> (x + 1, y + 1)
  (dx, dy) | dx < 0 && dy > 0     -> (x - 1, y + 1)
  (dx, dy) | dx > 0 && dy < 0     -> (x + 1, y - 1)
  (dx, dy) | dx < 0 && dy < 0     -> (x - 1, y - 1)
  -- IDK, it's probably fine
  _otherwise                      -> (x, y)

moveHead1 :: (Int, Int) -> Rope1 -> Rope1
moveHead1 (x, y) ((hx, hy), t) = ((hx + x, hy + y), t)


moveHead :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
moveHead (dx, dy) ((x, y):xs) = (x + dx, y + dy) : xs
moveHead _ []                 = []

followAll' n (x:xs) = follow n x : followAll' x xs
followAll' n []     = []

followAll (x:xs) = x : followAll' x xs
followAll []     = []


doStep1 s = follow' . moveHead1 s

doStep s = followAll . moveHead s

direction :: Parser Direction
direction = Left <$ "L" <|> Right <$ "R" <|> Up <$ "U" <|> Down <$ "D"

step :: Parser Step
step = do
  d <- direction
  " "
  n <- decimal
  return (d, n)

program = step `sepBy` "\n" <* endOfInput
