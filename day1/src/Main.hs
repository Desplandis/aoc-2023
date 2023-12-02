module Main where

import Data.Char (isDigit, digitToInt)

reencode :: String -> String
reencode [] = []
reencode ('o' : 'n' : xs@('e' : _)) = '1' : reencode xs
reencode ('t' : 'w' : xs@('o' : _)) = '2' : reencode xs
reencode ('t' : 'h' : 'r' : 'e' : xs@('e' : _)) = '3' : reencode xs
reencode ('f' : 'o' : 'u' : 'r' : xs) = '4' : reencode xs
reencode ('f' : 'i' : 'v' : xs@('e' : _)) = '5' : reencode xs
reencode ('s' : 'i' : 'x' : xs) = '6' : reencode xs
reencode ('s' : 'e' : 'v' : 'e' : xs@('n' : _)) = '7' : reencode xs
reencode ('e' : 'i' : 'g' : 'h' : xs@('t' : _)) = '8' : reencode xs
reencode ('n' : 'i' : 'n' : xs@('e' : _)) = '9' : reencode xs
reencode (x : xs) = x : reencode xs

line :: String -> [Int]
line [] = []
line xs = [ digitToInt x | x <- xs, isDigit x ]

calibration :: [Int] -> Int
calibration [] = 0
calibration xs = head xs * 10 + last xs

main :: IO ()
main =
  do
    input <- lines <$> getContents
    print $ sum $ map (calibration . line) input
    print $ sum $ map (calibration . line . reencode) input
