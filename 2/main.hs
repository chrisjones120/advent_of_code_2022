import System.Environment (getArgs)

data Move = Rock | Paper | Scissors deriving (Enum, Eq)

type Round = (Move, Move)

main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      print $ sum $ map (roundScore . toRound) (lines contents)
    _ -> putStrLn "Please provide a path to the input file."

toRound :: String -> Round
toRound s = (toMove a, toMove b)
  where
    a : b : rest = words s

toMove :: String -> Move
toMove s
  | s `elem` ["A", "X"] = Rock
  | s `elem` ["B", "Y"] = Paper
  | s `elem` ["C", "Z"] = Scissors

outcomeScore :: Round -> Int
outcomeScore r
  | roundWin r = 6
  | roundDraw r = 3
  | otherwise = 0

roundWin :: (Move, Move) -> Bool
roundWin r = r `elem` [(Rock, Paper), (Scissors, Rock), (Paper, Scissors)]

roundDraw :: Round -> Bool
roundDraw (a, b) = a == b

shapeScore :: Round -> Int
shapeScore (_, shape) = fromEnum shape + 1

roundScore :: Round -> Int
roundScore r = shapeScore r + outcomeScore r
