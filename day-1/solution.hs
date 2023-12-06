import Data.Char (isDigit)
import Data.Foldable (find)
import Data.Maybe (fromJust)

transform :: String -> String
transform ('o' : 'n' : 'e' : rest) = "1" ++ transform rest
transform ('t' : 'w' : 'o' : rest) = "2" ++ transform rest
transform ('t' : 'h' : 'r' : 'e' : 'e' : rest) = "3" ++ transform rest
transform ('f' : 'o' : 'u' : 'r' : rest) = "4" ++ transform rest
transform ('f' : 'i' : 'v' : 'e' : rest) = "5" ++ transform rest
transform ('s' : 'i' : 'x' : rest) = "6" ++ transform rest
transform ('s' : 'e' : 'v' : 'e' : 'n' : rest) = "7" ++ transform rest
transform ('e' : 'i' : 'g' : 'h' : 't' : rest) = "8" ++ transform rest
transform ('n' : 'i' : 'n' : 'e' : rest) = "9" ++ transform rest
transform (x : rest) = x : transform rest
transform [] = []

transformRev :: String -> String
transformRev ('e' : 'n' : 'o' : rest) = "1" ++ transformRev rest
transformRev ('o' : 'w' : 't' : rest) = "2" ++ transformRev rest
transformRev ('e' : 'e' : 'r' : 'h' : 't' : rest) = "3" ++ transformRev rest
transformRev ('r' : 'u' : 'o' : 'f' : rest) = "4" ++ transformRev rest
transformRev ('e' : 'v' : 'i' : 'f' : rest) = "5" ++ transformRev rest
transformRev ('x' : 'i' : 's' : rest) = "6" ++ transformRev rest
transformRev ('n' : 'e' : 'v' : 'e' : 's' : rest) = "7" ++ transformRev rest
transformRev ('t' : 'h' : 'g' : 'i' : 'e' : rest) = "8" ++ transformRev rest
transformRev ('e' : 'n' : 'i' : 'n' : rest) = "9" ++ transformRev rest
transformRev (x : rest) = x : transformRev rest
transformRev "" = ""

main = do
  input <- readFile "day-1/input.txt"
  let x = map (\x -> [fromJust $ find isDigit $ transform x, fromJust $ find isDigit $ transformRev $ reverse x]) $ lines input
  print $ sum $ map read x
