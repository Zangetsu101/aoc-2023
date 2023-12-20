import Data.Char (isDigit)
import Data.List (tails)
import qualified Data.Set as Set
import Control.Monad (mfilter)
import Data.Maybe (mapMaybe)

data Position = Position { row :: Int, start :: Int, end :: Int } deriving (Show)
data Number = Number { position :: Position, value :: Int  } deriving (Show)

extractNumbers2 :: Int -> Int -> String -> [Number]
extractNumbers2 rowIndex colIndex (x : xs) | (not . isDigit) x = extractNumbers2 rowIndex (colIndex + 1) xs
extractNumbers2 rowIndex colIndex (x : xs) = Number { position, value = read number } : extractNumbers2 rowIndex end (dropWhile isDigit xs)
  where number = x : takeWhile isDigit xs
        end = colIndex + length number
        position = Position { row = rowIndex, start = colIndex, end }
extractNumbers2 _  _ [] = []

extractStars :: Int -> Int -> String -> [(Int, Int)]
extractStars rowIndex colIndex (x : xs) | x == '*' = (rowIndex, colIndex) : extractStars rowIndex (colIndex + 1) xs
extractStars rowIndex colIndex (x : xs) = extractStars rowIndex (colIndex + 1) xs
extractStars _ _ [] = []

isNeighbour :: (Int, Int) -> Position -> Bool
isNeighbour (lrow, lcol) Position { row = rrow, start = rstart, end = rend } = abs (lrow - rrow) <= 1 && (rstart - 1 <= lcol && rend >= lcol)

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

part1 :: [String] -> Int
part1 input = sum $ concatMap (\(s, numbers) -> mapMaybe (\((start, end), number) -> mfilter (< end) (Set.lookupGE start s) >> Just number) numbers) pairs
  where pairs = zip validPositions $ map (extractNumbers 0 . (!!1)) windows
        windows = take lineLength $ map (take 3) $ tails lines'
        validPositions = map (Set.fromList . concatMap (extractValidPositions 0)) windows
        lineLength = length $ head input
        lines' = [replicate lineLength '.'] ++ input ++ [replicate lineLength '.']

part2 input = sum gears
  where numbers = concatMap (\(row, line) -> extractNumbers2 row 0 line) $ zip [0..] input
        stars = concatMap (\(row, line) -> extractStars row 0 line) $ zip [0..] input
        neighbourNumbers = map (\star -> filter (\ Number { position = p } -> star `isNeighbour` p) numbers) stars
        gears = map (foldl (\acc Number { value = v } -> acc * v) 1 ) $ filter ((== 2) . length) neighbourNumbers

main = do
  input <- readFile "day-3/input.txt"
  let inputLines = lines input
  print $ part1 inputLines
  print $ part2 inputLines
