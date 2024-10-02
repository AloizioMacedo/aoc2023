{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS -fno-implicit-prelude #-}

module Main where

import BasicPrelude as Bp

main :: IO ()
main = do
  contents <- Bp.readFile "input.txt"
  print $ solvePartOne contents
  print solvePartTwo

-- | First int is waitTime, second is totalTime.
getDistance :: Int -> Int -> Int
getDistance waitTime totalTime = waitTime * (totalTime - waitTime)

data Race = Race {time :: Int, distance :: Int} deriving (Show)

totalWaysToWin :: Race -> Int
totalWaysToWin r = totalWaysToWin' (time r) r

totalWaysToWin' :: Int -> Race -> Int
totalWaysToWin' t r@Race {time, distance}
  | t == 0 = 0
  | otherwise = if getDistance t time >= distance then 1 + totalWaysToWin' (t - 1) r else totalWaysToWin' (t - 1) r

separateContents :: Text -> ([Text], [Text])
separateContents c = case Bp.lines c of
  [x, y] -> (filter (isJust . (readMay @Int)) (Bp.words x), filter (isJust . readMay @Int) (Bp.words y))
  _ -> error "could not separate contents"

parseRaces :: ([Text], [Text]) -> [Race]
parseRaces (t, s) = Bp.map parseRace (zip t s)

parseRace :: (Text, Text) -> Race
parseRace (x, y) = Race {time = Bp.read x, distance = Bp.read y}

solvePartOne :: Text -> Int
solvePartOne s = product $ map totalWaysToWin ((parseRaces . separateContents) s)

solvePartTwo :: Int
solvePartTwo = totalWaysToWin Race {time = 40817772, distance = 219101213651089}