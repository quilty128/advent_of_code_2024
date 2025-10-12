module Main where

splitRow :: String -> [Int]
splitRow rowStr =
  let xs = words rowStr
   in map read xs :: [Int]

isSafe1 :: [Int] -> Bool
isSafe1 row =
  let differences = zipWith (\x y -> x - y) row (drop 1 row)
   in (all (\x -> 1 <= x && x <= 3) differences)
        || (all (\x -> -3 <= x && x <= -1) differences)

part1 :: [[Int]] -> Int
part1 rows = length $ filter isSafe1 rows

isSafe2 :: [Int] -> Bool
isSafe2 row = or $ map isSafe1 removalPermutations
  where
    len = length row
    removalPermutations = map (\i -> (take i row) ++ (drop (i + 1) row)) [0 .. len]

part2 :: [[Int]] -> Int
part2 rows = length $ filter isSafe2 rows

main :: IO ()
main = do
  contents <- readFile "input/day2.txt"
  let rows = map splitRow $ lines contents
  putStr "Part 1: "
  print $ part1 rows
  putStr "Part 2: "
  print $ part2 rows
