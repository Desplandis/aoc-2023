module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


data Card
  = Card { winning :: Set Int, numbers :: Set Int }
  deriving Show


-- Part 1
matchingCount :: Card -> Int
matchingCount = Set.size . matchingNumbers
  where
    matchingNumbers = Set.intersection . winning <*> numbers

matchingPoints :: Card -> Int
matchingPoints = countPoints . matchingCount
  where
    countPoints 0 = 0
    countPoints n = 2 ^ (n - 1)


-- Part 2
emittedCounts :: [Card] -> [Int]
emittedCounts [] = []
emittedCounts (c : cs) = 1 + sumTo (matchingCount c) counts : counts
  where
    counts = emittedCounts cs
    sumTo :: Int -> [Int] -> Int
    sumTo _ [] = 0
    sumTo 0 _  = 0
    sumTo n (x: xs) = x + sumTo (n - 1) xs


-- Parser
type Parser = Parsec Void String

parseCard :: Parser Card
parseCard = id *> card
  where
    id :: Parser Int
    id = string "Card" <* hspace1 *> L.decimal <* single ':' <* hspace
    card :: Parser Card
    card = Card <$> numbers <* separator <*> numbers
    numbers :: Parser (Set Int)
    numbers = Set.fromList <$> sepEndBy1 L.decimal hspace1
    separator = hspace *> char '|' <* hspace

parseCards :: Parser [Card]
parseCards = many $ parseCard <* newline


-- Main
main :: IO ()
main =
  do
    input <- getContents
    case parse parseCards "" input of
      Left error -> putStr $ errorBundlePretty error
      Right cards -> do
        putStr "Total points from winning cards: "
        print $ sum $ matchingPoints <$> cards
        putStr "Total points from winning emitted cards: "
        print $ sum $ emittedCounts cards
