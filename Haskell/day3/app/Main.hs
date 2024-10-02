{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BasicPrelude (groupBy)
import Data.Char (digitToInt, isDigit)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ solvePartOne contents

data Number = Number {col1 :: Int, col2 :: Int, line :: Int, val :: Int} deriving (Show)

data Gear = Gear {linep :: Int, colp :: Int, symbol :: Char} deriving (Show)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

groupNumbers :: String -> [[(Int, Char)]]
groupNumbers v = groupBy (\(_, a) (_, b) -> isDigit a && isDigit b) (enumerate v)

isNumber :: [(Int, Char)] -> Bool
isNumber [] = True
isNumber ((_, c) : _) = isDigit c

isGear :: [(Int, Char)] -> Bool
isGear [] = False
isGear ((_, c) : _) = not (isDigit c) && c /= '.'

-- | Assumes isNumber is true.
toNumber :: (Int, [(Int, Char)]) -> Number
toNumber (_, []) = error "not valid number"
toNumber (l, [(c, x)]) = Number {col1 = c, col2 = c, line = l, val = digitToInt x}
toNumber (l, (c, x) : xs) = let n = toNumber (l, xs) in Number {col1 = c, col2 = col2 n, line = l, val = (10 ^ length xs) * digitToInt x + val n}

-- | Assumes isGear is true.
toGear :: (Int, [(Int, Char)]) -> Gear
toGear (_, []) = error "not valid number"
toGear (l, [(c, s)]) = Gear {linep = l, colp = c, symbol = s}
toGear _ = error "not a gear really"

groupsNumbersInAllLines :: [String] -> [(Int, [[(Int, Char)]])]
groupsNumbersInAllLines v = enumerate (map groupNumbers v)

pack :: (a, [b]) -> [(a, b)]
pack (_, []) = []
pack (x, y : ys) = (x, y) : pack (x, ys)

getAllNumbers :: [String] -> [Number]
getAllNumbers v = concatMap (map toNumber . filter (\(_, n) -> isNumber n) . pack) (groupsNumbersInAllLines v)

getAllGears :: [String] -> [Gear]
getAllGears v = concatMap (map toGear . filter (\(_, n) -> isGear n) . pack) (groupsNumbersInAllLines v)

isClose :: Number -> Gear -> Bool
isClose Number {col1, col2, line} Gear {linep, colp} = colp >= col1 - 1 && colp <= col2 + 1 && linep >= line - 1 && linep <= line + 1

isCloseToAGear :: Number -> [Gear] -> Bool
isCloseToAGear n = any (isClose n)

solvePartOne :: String -> Int
solvePartOne s =
  let numbers = getAllNumbers $ lines s
      gears = getAllGears $ lines s
   in sum $ map val (filter (`isCloseToAGear` gears) numbers)