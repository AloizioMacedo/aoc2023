data Set = Set {red :: Int, green :: Int, blue :: Int}

newtype Game = Game {sets :: [Set]}

split :: Char -> String -> [String]
split = split' [] []

split' :: [String] -> String -> Char -> String -> [String]
split' l current c [] = l ++ [current]
split' l current c (y : ys) = if c == y then split' (l ++ [current]) [] c ys else split' l (current ++ [y]) c ys

-- parseLine :: String -> Set
-- parseLine x = split (== ':') x

-- separateId :: String -> (Int, String)
-- separateId x = case splitOn ":" x of
--   [a, b] -> case splitOn " " a of
--     [d, e] -> (read e :: Int, b)
--     _ -> error "could not parse id"
--   _ -> error "could not separate id"

-- spaces :: String -> [String]
-- spaces x = splitOn " " x