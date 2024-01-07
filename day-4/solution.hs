findWinningCards card = sum $ map (fromEnum . (`elem` winningNumbers)) ownedNumbers
 where
  numbers = tail . dropWhile (/= ':') $ card
  winningNumbers = map (read :: String -> Int) . words . takeWhile (/= '|') $ numbers
  ownedNumbers = map (read :: String -> Int) . words . tail . dropWhile (/= '|') $ numbers

part2 (card : cards) (count : counts) = count : part2 cards newCounts
 where
  newCounts = map (+ count) (take winningCards counts) ++ drop winningCards counts
  winningCards = findWinningCards card
part2 [] _ = []
part2 _ [] = []

main = do
  input <- readFile "day-4/input.txt"
  let cards = lines input
  print $ sum $ map ((2 ^) . subtract 1) $ filter (/= 0) $ map findWinningCards cards
  print $ sum $ part2 cards (repeat 1)
