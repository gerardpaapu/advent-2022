#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Function        ((&))
import           Data.List            as L
import           Data.Text
import           Data.Text.IO         as Text

main :: IO ()
main = do
  txt <- Text.readFile "input.txt"
  commands <- either fail return $ parseOnly (program <* endOfInput) txt

  let state = Zipper [] "/" []
  let tree = toSize $ fromZipper $ L.foldl' (flip run) state commands
  let part1 = tree
        & dirs
        & L.filter (\(name, size) -> size <= 100000)
        & fmap snd
        & sum

  let used = getSize tree
  let freespace = 70000000 - used
  let needToClear = 30000000 - freespace
  let part2 = tree
        & dirs
        & L.filter (\(name, size) -> size >= needToClear)
        & L.sortOn snd
        & L.head
        & snd
  print part1
  print part2

data SizeE = DirS Text Int ![SizeE] | FileS Int
  deriving Show

data Entry = Dir !Text ![Entry] | File !Text !Int
  deriving (Show, Eq)
data Cmd = Up | Down !Text | Ls ![Entry] | Root
  deriving Show

data Zipper = Zipper [Entry] Text [Entry]
run Up z       = up' z
run (Down t) z = case down' t z of
  Just r  -> r
  Nothing -> error "shit"
run Root z     = root' z
run (Ls c) z   = ls' c z

down' dirname (Zipper xs name c) = do
  children <- firstJust (withName dirname) c
  return $ Zipper (Dir name c : xs) dirname children
  where
    withName n m = case m of
      Dir nn a | nn == n -> Just a
      _                  -> Nothing


toSize (File _ s) = FileS s
toSize (Dir name es) = DirS name total cs
  where
    cs = toSize <$> es

    total = sum (getSize <$> cs)

getSize (FileS n)    = n
getSize (DirS _ n _) = n

dirs (DirS name total xs) = (name, total) : L.concatMap dirs xs
dirs _                    = []

up' (Zipper (x:xs) name children) = case x of
  Dir n c -> do
    let c' = fmap (replace name children) c
    Zipper xs n c'
  _ -> error "idk wtf"
  where
    replace name a b = case b of
      Dir n _ | n == name -> Dir n a
      _otherwise          -> b

up' _ = error "Can't go up from here"

root' (Zipper [] n c) = Zipper [] n c
root' z               = root' $ up' z

ls' children (Zipper ls name _)  = Zipper ls name children

fromZipper (Zipper [] n c) = Dir n c
fromZipper z               = fromZipper $ up' z

firstJust f []     = Nothing
firstJust f (x:xs) = case f x of
  Just v  -> Just v
  Nothing -> firstJust f xs

program :: Parser [Cmd]
program = command `sepBy` "\n"

command :: Parser Cmd
command = up <|> down <|> ls <|> root

root = Root <$ "$ cd /"
up = Up <$ "$ cd .."
down = Down . pack <$> ("$ cd " *> many1 letter)
ls = Ls <$> ("$ ls\n" *> entry `sepBy` "\n")
dir = flip Dir [] . pack <$> ("dir " *> many1 letter)
file = do
  size <- decimal
  " "
  name <- many1 (letter <|> char '.')
  return $ File (pack name) size
entry = file <|> dir


