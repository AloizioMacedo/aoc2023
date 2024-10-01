import Distribution.Utils.String (trim)

main = do
  contents <- readFile "input.txt"
  print $ solvePartOne contents
  print $ solvePartTwo contents

data Set = Set {red :: Int, green :: Int, blue :: Int} deriving (Show)

data Game = Game {idx :: Int, sets :: [Set]} deriving (Show)

split :: Char -> String -> [String]
split = split' [] []

split' :: [String] -> String -> Char -> String -> [String]
split' l current c [] = l ++ [current]
split' l current c (y : ys) = if c == y then split' (l ++ [current]) [] c ys else split' l (current ++ [y]) c ys

parseLine :: String -> Game
parseLine x = case separateId x of
  (i, s) -> Game {idx = i, sets = map getSet (split ';' s)}

separateId :: String -> (Int, String)
separateId x = case split ':' x of
  [a, b] -> case split ' ' a of
    [d, e] -> (read e :: Int, b)
    _ -> error "could not parse id"
  _ -> error "could not separate id"

getSet :: String -> Set
getSet s = getSet' (Set 0 0 0) (split ',' s)

getSet' :: Set -> [String] -> Set
getSet' set [] = set
getSet' set (x : xs) = case split ' ' (trim x) of
  [a, "red"] -> getSet' (Set {red = read (trim a) :: Int, green = green set, blue = blue set}) xs
  [a, "green"] -> getSet' (Set {red = red set, green = read (trim a) :: Int, blue = blue set}) xs
  [a, "blue"] -> getSet' (Set {red = red set, green = green set, blue = read (trim a)}) xs
  _ -> set

isPossible :: Set -> Bool
isPossible (Set {red, green, blue}) = red <= 12 && green <= 13 && blue <= 14

isPossibleGame :: Game -> Bool
isPossibleGame (Game {sets}) = all isPossible sets

getMaxSet :: Game -> Set
getMaxSet (Game {sets}) = Set {red = foldl max 0 (map red sets), green = foldl max 0 (map green sets), blue = foldl max 0 (map blue sets)}

getPower :: Set -> Int
getPower (Set x y z) = x * y * z

solvePartOne :: String -> Int
solvePartOne contents = sum $ map idx (filter isPossibleGame (map parseLine (lines contents)))

solvePartTwo :: String -> Int
solvePartTwo contents = sum $ map (getPower . getMaxSet . parseLine) (lines contents)