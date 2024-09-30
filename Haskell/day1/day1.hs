import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

main = do
  contents <- readFile "input.txt"
  print $ solvePartOne contents
  print $ solvePartTwo contents

getFirstInt :: String -> Int
getFirstInt [] = 0
getFirstInt (x : xs) = if isDigit x then digitToInt x else getFirstInt xs

getLastInt :: String -> Int
getLastInt x = getFirstInt $ reverse x

parseLine :: String -> Int
parseLine x = 10 * getFirstInt x + getLastInt x

solvePartOne :: String -> Int
solvePartOne x = sum $ map parseLine (lines x)

getFirstInt2 :: String -> Int
getFirstInt2 [] = 0
getFirstInt2 (x : xs)
  | isDigit x = digitToInt x
  | "one" `isPrefixOf` (x : xs) = 1
  | "two" `isPrefixOf` (x : xs) = 2
  | "three" `isPrefixOf` (x : xs) = 3
  | "four" `isPrefixOf` (x : xs) = 4
  | "five" `isPrefixOf` (x : xs) = 5
  | "six" `isPrefixOf` (x : xs) = 6
  | "seven" `isPrefixOf` (x : xs) = 7
  | "eight" `isPrefixOf` (x : xs) = 8
  | "nine" `isPrefixOf` (x : xs) = 9
  | otherwise = getFirstInt2 xs

getIntReversed :: String -> Int
getIntReversed [] = 0
getIntReversed (x : xs)
  | isDigit x = digitToInt x
  | "eno" `isPrefixOf` (x : xs) = 1
  | "owt" `isPrefixOf` (x : xs) = 2
  | "eerht" `isPrefixOf` (x : xs) = 3
  | "ruof" `isPrefixOf` (x : xs) = 4
  | "evif" `isPrefixOf` (x : xs) = 5
  | "xis" `isPrefixOf` (x : xs) = 6
  | "neves" `isPrefixOf` (x : xs) = 7
  | "thgie" `isPrefixOf` (x : xs) = 8
  | "enin" `isPrefixOf` (x : xs) = 9
  | otherwise = getIntReversed xs

getLastInt2 :: String -> Int
getLastInt2 x = getIntReversed $ reverse x

parseLine2 :: String -> Int
parseLine2 x = 10 * getFirstInt2 x + getLastInt2 x

solvePartTwo :: String -> Int
solvePartTwo x = sum $ map parseLine2 (lines x)