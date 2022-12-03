import System.Environment (getArgs)

data Move = Rock | Paper | Scissors deriving (Enum, Eq)

data Outcome = Lose | Draw | Win deriving (Enum, Eq)

type Strategy = (Move, Outcome)

main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      print $ sum $ map (roundScore . toStrategy) (lines contents)
    _ -> putStrLn "Please provide a path to the input file."

toStrategy :: String -> Strategy
toStrategy s = (toMove a, toOutcome b)
  where
    a : b : rest = words s

toMove :: String -> Move
toMove s = case s of
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors

toOutcome :: String -> Outcome
toOutcome s = case s of
  "X" -> Lose
  "Y" -> Draw
  "Z" -> Win

chooseMove :: (Move, Outcome) -> Move
chooseMove s
  | s `elem` [(Paper, Win), (Rock, Lose)] = Scissors
  | s `elem` [(Rock, Win), (Scissors, Lose)] = Paper
  | s `elem` [(Scissors, Win), (Paper, Lose)] = Rock
  | otherwise = fst s

outcomeScore :: Outcome -> Int
outcomeScore o = fromEnum o * 3

moveScore :: Move -> Int
moveScore m = fromEnum m + 1

roundScore :: Strategy -> Int
roundScore s = moveScore (chooseMove s) + outcomeScore (snd s)
