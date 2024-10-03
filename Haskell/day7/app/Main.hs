{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasicPrelude
import Data.Text (splitOn, unpack)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ solvePartOne contents

newtype Hand = Hand [Char] deriving (Eq)

getValue :: Char -> Int
getValue '2' = 2
getValue '3' = 3
getValue '4' = 4
getValue '5' = 5
getValue '6' = 6
getValue '7' = 7
getValue '8' = 8
getValue '9' = 9
getValue 'T' = 10
getValue 'J' = 11
getValue 'Q' = 12
getValue 'K' = 13
getValue 'A' = 14
getValue _ = -1

isFiveOfAKind :: Hand -> Bool
isFiveOfAKind (Hand h) = all (\c -> c == head h) h

isFourOfAKind :: Hand -> Bool
isFourOfAKind (Hand h) = any (\c -> length c == 4) ((group . sort) h)

isFullHouse :: Hand -> Bool
isFullHouse (Hand h) = length ((group . sort) h) == 2 && ((length (head ((group . sort) h)) == 3) || (length (head ((group . sort) h)) == 2))

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind (Hand h) = any (\c -> length c == 3) ((group . sort) h)

isTwoPair :: Hand -> Bool
isTwoPair (Hand h) = length (filter (\c -> length c == 2) ((group . sort) h)) == 2

isOnePair :: Hand -> Bool
isOnePair (Hand h) = length (filter (\c -> length c == 2) ((group . sort) h)) == 1

tieBreak :: Hand -> Hand -> Ordering
tieBreak (Hand [x]) (Hand [y])
  | getValue x > getValue y = GT
  | getValue x < getValue y = LT
  | getValue x == getValue y = EQ
tieBreak (Hand (x : xs)) (Hand (y : ys))
  | getValue x > getValue y = GT
  | getValue x < getValue y = LT
  | getValue x == getValue y = tieBreak (Hand xs) (Hand ys)
tieBreak _ _ = error "weird case of tieBreak"

getScore :: Hand -> Int
getScore h
  | isFiveOfAKind h = 6
  | isFourOfAKind h = 5
  | isFullHouse h = 4
  | isThreeOfAKind h = 3
  | isTwoPair h = 2
  | isOnePair h = 1
  | otherwise = 0

instance Ord Hand where
  compare h1 h2
    | getScore h1 < getScore h2 = LT
    | getScore h1 > getScore h2 = GT
    | getScore h1 == getScore h2 = tieBreak h1 h2
    | otherwise = error "unreachable"

lineToHandAndBit :: Text -> (Hand, Int)
lineToHandAndBit t = case splitOn " " t of
  [x, y] -> (Hand (unpack x), read y)
  _ -> error "could not parse line"

toHands :: Text -> [(Hand, Int)]
toHands t = map lineToHandAndBit (lines t)

sortHands :: [(Hand, Int)] -> [(Hand, Int)]
sortHands = sortBy (\(x, _) (y, _) -> compare x y)

solvePartOne :: Text -> Int
solvePartOne t = foldl (\acc (p, q) -> acc + p * q) 0 (zip (map snd ((sortHands . toHands) t)) [1 ..])