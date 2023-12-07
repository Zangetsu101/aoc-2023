import Data.Char (isDigit)
import Data.List (tails)
import qualified Data.Set as Set
import Control.Monad (mfilter)
import Data.Maybe (mapMaybe)

extractNumbers :: Int -> String -> [((Int, Int), Int)]
extractNumbers idx (x : xs) | (not . isDigit) x = extractNumbers (idx + 1) xs
extractNumbers idx (x : xs) = do
  let number = x : takeWhile isDigit xs
  let end = idx + length number
  ((idx, end), read number) : extractNumbers end (dropWhile isDigit xs)
extractNumbers _ [] = []

extractValidPositions :: Int -> String -> [Int]
extractValidPositions idx (x : xs) | isDigit x = extractValidPositions (idx + 1) xs
extractValidPositions idx ('.' : xs) = extractValidPositions (idx + 1) xs
extractValidPositions idx (x : xs) = (idx - 1) : idx : (idx + 1) : extractValidPositions (idx + 1) xs
extractValidPositions _ [] = []

main = do
  input <- readFile "day-3/input.txt"
  let inputLines = lines input
  let lineLength = length $ head inputLines
  let lines' = [replicate lineLength '.'] ++ inputLines ++ [replicate lineLength '.']
  let windows = take lineLength $ map (take 3) $ tails lines'
  let validPositions = map (Set.fromList . concatMap (extractValidPositions 0)) windows
  let pairs = zip validPositions $ map (extractNumbers 0 . (!!1)) windows
  print $ sum $ concatMap (\(s, numbers) -> mapMaybe (\((start, end), number) -> mfilter (< end) (Set.lookupGE start s) >> Just number) numbers)  pairs
