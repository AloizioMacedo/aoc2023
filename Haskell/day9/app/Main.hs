{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import BasicPrelude
import Data.Text (splitOn)

main :: IO ()
main =
  do
    contents <- readFile "input.txt"
    print $ solvePartOne contents
    print $ solvePartTwo contents

forecast :: [Int] -> Int
forecast l = tearDown (buildUp [l])

buildUp :: [[Int]] -> [[Int]]
buildUp l
  | all (== 0) (head l) = l
  | otherwise = buildUp $ getDiffs (head l) : l

getDiffs :: [Int] -> [Int]
getDiffs l = zipWith (-) (tail l) l

tearDown :: [[Int]] -> Int
tearDown = tearDown' 0

tearDown' :: Int -> [[Int]] -> Int
tearDown' = foldl (\acc y -> acc + last y)

parse :: Text -> [Int]
parse t = map read (splitOn " " t)

solvePartOne :: Text -> Int
solvePartOne t = sum $ map (forecast . parse) (lines t)

solvePartTwo :: Text -> Int
solvePartTwo t = sum $ map (forecast . reverse . parse) (lines t)