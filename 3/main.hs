import Data.Char (ord)
import Data.List (elemIndex, group)
import System.Environment (getArgs)

type Rucksack = (String, String)

main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      putStrLn "Incorrectly sorted items: "
      print $ sum $ map (priority . sharedItem . rucksack) $ lines contents
      putStrLn "\n Group badges: "
      print $ sum $ map priority . badges $ map rucksack $ lines contents
    _ -> putStrLn "Please provide a path to the input file."

rucksack :: String -> Rucksack
rucksack s = splitAt (length s `div` 2) s

uniq :: [Char] -> [Char]
uniq = map head . group

sharedItem :: Rucksack -> Char
sharedItem (a, b) = i
  where
    [i] = uniq $ filter (`elem` b) a

groupBadge :: Rucksack -> Rucksack -> Rucksack -> Char
groupBadge a b c = i
  where
    [i] = uniq $ filter (`elem` all' c) $ filter (`elem` all' b) $ all' a

priority :: Char -> Int
priority c = i + 1
  where
    Just i = c `elemIndex` (['a' .. 'z'] ++ ['A' .. 'Z'])

all' :: Rucksack -> [Char]
all' (l, r) = l ++ r

badges :: [Rucksack] -> [Char]
badges r = f r []
  where
    f [] agg = agg
    f (a : b : c : xs) agg =
      f xs $ groupBadge a b c : agg
