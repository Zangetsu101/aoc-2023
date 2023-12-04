parseCube::String -> (Int, Int, Int)
parseCube cubeInput = case break (== ' ') cubeInput of
  (count, _:color) -> case color of 
    "red" -> (read count, 0, 0)
    "green" -> (0, read count, 0)
    "blue" -> (0, 0, read count)
    _ -> (0, 0, 0)
  _ -> (0, 0, 0)

max'::(Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
max' (a, b, c) (d, e, f) = (max a d, max b e, max c f)

parseSet::String -> (Int, Int, Int)
parseSet setInput = case break (== ',') setInput of
  (cubeOne, _:_:restOfCubes) -> parseCube cubeOne `max'` parseSet restOfCubes
  (cube, []) -> parseCube cube

parseGame::String -> (Int, Int, Int)
parseGame gameInput = case break (== ';') gameInput of
  (setOne, _:_:restOfSets) -> parseSet setOne `max'` parseGame restOfSets
  (set, []) -> parseSet set

parseId::String -> Int
parseId gameId = case break (== ' ') gameId of
  (_, _:id) -> read id
  _ -> 0

parseGameWithId::String -> (Int, (Int, Int, Int))
parseGameWithId gameWithId = case break (== ':') gameWithId of
  (gameId, _:_:game) -> (parseId gameId, parseGame game)
  _ -> (0, (0, 0, 0))

main = do
  input <- readFile "day-2/input.txt"
  print $ sum $ map fst $ filter (\(id, (r, g, b)) -> r <= 12 && g <= 13 && b <= 14) $ map parseGameWithId $ lines input
