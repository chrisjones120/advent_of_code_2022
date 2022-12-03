import Data.ByteString as BS (ByteString, empty, readFile, split)
import Data.ByteString.Char8 (readInt)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- BS.readFile path
      print $ maximum $ map sum $ filter (/= []) $ splitElves $ split 10 contents
    _ -> putStrLn "Please provide a path to the input file."

asCalories :: ByteString -> Int
asCalories = getVal . readInt

getVal :: Maybe (a, b) -> a
getVal maybe = x
  where
    Just (x, y) = maybe

splitElves :: [ByteString] -> [[Int]]
splitElves xs = f xs []
  where
    f [] agg = [agg]
    f (y : ys) agg =
      if y == BS.empty
        then agg : f ys []
        else f ys (asCalories y : agg)
