module Main where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Set = (Int, Int, Int)
type Game = (Int, [Set])

-- Part 1
possible :: (Set -> Bool) -> Game -> Bool
possible pred (_, ds) = all pred ds

sumPossible :: (Set -> Bool) -> [Game] -> Int
sumPossible p g = sum $ fst <$> filter (possible p) g

-- Part 2
maxSet :: Game -> Set
maxSet (_, ds) = foldl maxC (0, 0, 0) ds
  where
    maxC (r, g, b) (r', g', b') = (max r r', max g g', max b b')

power :: Set -> Int
power (r, g, b) = r * g * b

sumMaxSet :: [Game] -> Int
sumMaxSet g = sum $ power . maxSet <$> g

-- Parser
type Parser = Parsec Void String

parseGames :: Parser [Game]
parseGames = many $ record <* eol
  where
    record = (,) <$> (string "Game " *> L.decimal <* string ": ") <*> game
    game = set `sepBy` string "; "
    set = foldl addC (0, 0, 0) <$> subset `sepBy` string ", "
    addC (x, y, z) (x', y', z') = (x + x', y + y', z + z')
    subset =
      L.decimal <* space >>= \i ->
            (i, 0, 0) <$ string "red"
        <|> (0, i, 0) <$ string "green"
        <|> (0, 0, i) <$ string "blue"

-- Main
main :: IO ()
main =
  do
    input <- getContents
    case parse parseGames "" input of
      Left bundle -> print $ bundleErrors bundle
      Right d -> do
        print $ sumPossible (\(r, g, b) -> r <= 12 && g <= 13 && b <= 14 ) d
        print $ sumMaxSet d
