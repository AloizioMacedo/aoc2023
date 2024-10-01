{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Main where

import qualified BasicPrelude as Bp
import Data.Text (Text, splitOn, strip)

main :: IO ()
main = do
  contents <- Bp.readFile "input.txt"
  print $ solvePartOne contents
  print $ solvePartTwo contents

data Card = Card {i :: Text, winningNumbers :: [Int], myNumbers :: [Int]} deriving (Show)

countWinning :: Card -> Int
countWinning Card {winningNumbers, myNumbers} = countWinning' 0 winningNumbers myNumbers

countWinning' :: Int -> [Int] -> [Int] -> Int
countWinning' c _ [] = c
countWinning' c x (y : ys) = if y `elem` x then countWinning' (c + 1) x ys else countWinning' c x ys

getScoreFromCount :: Int -> Int
getScoreFromCount 0 = 0
getScoreFromCount 1 = 1
getScoreFromCount x = 2 * getScoreFromCount (x - 1)

splitCardId :: Text -> (Text, Text)
splitCardId s = case splitOn ":" s of
  [x, y] -> (strip x, strip y)
  _ -> error "failed splitting"

splitWinningAndMine :: Text -> (Text, Text)
splitWinningAndMine s = case splitOn "|" s of
  [x, y] -> (strip x, strip y)
  _ -> error "failed splitting"

parseNumbers :: Text -> [Int]
parseNumbers s = map (Bp.read . strip) $ filter (/= "") (splitOn " " s)

parseLine :: Text -> Card
parseLine l =
  let (i, c) = splitCardId l
      (winning, mine) = splitWinningAndMine c
      winningNumbers = parseNumbers winning
      myNumbers = parseNumbers mine
   in Card {i, winningNumbers, myNumbers}

solvePartOne :: Text -> Int
solvePartOne s = sum $ map (getScoreFromCount . countWinning . parseLine) $ Bp.lines s

transformCardsToCountedCards :: [Card] -> [(Card, Int)]
transformCardsToCountedCards = map (\x -> (x, 1))

resolve :: [(Card, Int)] -> [(Card, Int)]
resolve l = resolve' l []

resolve' :: [(Card, Int)] -> [(Card, Int)] -> [(Card, Int)]
resolve' [] cardsBuilt = reverse cardsBuilt
resolve' ((c, i) : cardsLeft) cardsBuilt =
  let matching = countWinning c
   in resolve' (zipWith (curry (\(n, (x, j)) -> (x, n + j))) (replicate matching i ++ repeat 0) cardsLeft) ((c, i) : cardsBuilt)

countCards :: [(Card, Int)] -> Int
countCards l = sum $ map snd l

solvePartTwo :: Text -> Int
solvePartTwo s = (countCards . resolve . transformCardsToCountedCards) $ map parseLine (Bp.lines s)
