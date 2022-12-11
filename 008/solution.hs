#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

import           Data.Attoparsec.Text
import           Data.List
import qualified Data.Text.IO         as T


main :: IO ()
main = do
  txt <- T.readFile "input.txt"
  -- let txt = "30373\n\
  --           \25512\n\
  --           \65332\n\
  --           \33549\n\
  --           \35390"
  grid <- either fail return $ parseOnly (grid' <* endOfInput) txt
  let withCoords = zipWith (\n ls -> zipWith (\a b -> (a, n, b)) [1..] ls) [1..] grid
  let a = withCoords -- left to right
  let b = transpose withCoords -- top to bottom
  let c = reverse <$> withCoords -- right to left
  let d = reverse <$> transpose withCoords -- bottom to top
  let visible = nub $ getVisible a ++ getVisible b ++ getVisible c ++ getVisible d
  -- -- print a
  -- -- let visible = getVisible a

  -- let o = fmap (fmap (\x -> if x `elem` visible then '_' else '#' )) withCoords

  -- let diagram = intercalate "\n" o ++ "\n"
  -- putStr diagram
  -- print $ getVisible d
  print $ length visible

  let horz = concat $ fmap zips grid
  let vert = concat $ transpose $ fmap zips (transpose grid)

  let part2 = maximum $ fmap (\((a, b, c), (d, e, f)) -> product $ rowViewDistance b <$> [a, c, d, f]) $ zip horz vert
  print part2
  -- print $  (zips [1, 2, 3, 4, 5]) !! 2

grid' :: Parser [[Int]]
grid' = many1 readDigit `sepBy1` "\n"
  where
    readDigit :: Parser Int
    readDigit = read . (:  []) <$> digit

height :: (a, b, Int) -> Int
height (_, _, n) = n


getVisible = concatMap (scanRow (-1) [])

scanRow h seen (x:xs) =
  if height x > h then
    scanRow (height x) (x : seen) xs
  else
    scanRow h seen xs
scanRow _ seen [] = reverse seen

rowViewDistance h [] = 0
rowViewDistance h (x:xs) =
  if x >= h then
    1
  else
    1 + rowViewDistance h xs

rowViewDistance' (x:xs) = rowViewDistance x xs
rowViewDistance' _      = error "fucked"

viewDistance x y g = do
  let (a, b, c) = zips (g !! y) !! x
  let (a', b', c') = zips (transpose g !! x) !! y
  (rowViewDistance b a, rowViewDistance b c, rowViewDistance b' a', rowViewDistance b' c')

zips' prev (x:xs) = (prev, x, xs) : zips' (x : prev) xs
zips' prev []     = []

zips = zips' []
