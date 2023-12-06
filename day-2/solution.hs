import Data.Maybe (fromJust)

parseCube :: String -> Maybe (Int, Int, Int)
parseCube cubeInput = case break (== ' ') cubeInput of
  (count, _ : color) -> case color of
    "red" -> Just (read count, 0, 0)
    "green" -> Just (0, read count, 0)
    "blue" -> Just (0, 0, read count)
    _ -> Nothing
  _ -> Nothing

max' :: Maybe (Int, Int, Int) -> Maybe (Int, Int, Int) -> Maybe (Int, Int, Int)
max' (Just (a, b, c)) (Just (d, e, f)) = Just (max a d, max b e, max c f)
max' _ _ = Nothing

parseSet :: String -> Maybe (Int, Int, Int)
parseSet setInput = case break (== ',') setInput of
  (cubeOne, _ : _ : restOfCubes) -> parseCube cubeOne `max'` parseSet restOfCubes
  (cube, []) -> parseCube cube

parseGame :: String -> Maybe (Int, Int, Int)
parseGame gameInput = case break (== ';') gameInput of
  (setOne, _ : _ : restOfSets) -> parseSet setOne `max'` parseGame restOfSets
  (set, []) -> parseSet set

parseId :: String -> Int
parseId gameId = case break (== ' ') gameId of
  (_, _ : id) -> read id
  _ -> 0

parseGameWithId :: String -> (Int, (Int, Int, Int))
parseGameWithId gameWithId = case break (== ':') gameWithId of
  (gameId, _ : _ : game) -> (parseId gameId, fromJust $ parseGame game)
  _ -> (0, (0, 0, 0))

main = do
  input <- readFile "day-2/input.txt"
  print $ sum $ map fst $ filter (\(id, (r, g, b)) -> r <= 12 && g <= 13 && b <= 14) $ map parseGameWithId $ lines input
  print $ sum $ map ((\(r, g, b) -> r * g * b) . snd . parseGameWithId) $ lines input
