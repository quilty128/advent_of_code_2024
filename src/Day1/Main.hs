module Main where

import Data.Char (isSpace)
import Data.List (sort)

trimStart :: String -> String
trimStart str = dropWhile isSpace str

splitOnSpace :: String -> (Int, Int)
splitOnSpace str =
  let 
    (col1, col2) = span (not . isSpace) str
  in
    (read $ trimStart col1, read $ trimStart col2) :: (Int, Int)

distance :: Int -> Int -> Int
distance x y = abs (x - y)

similarityScore :: Int -> [Int] -> Int
similarityScore n xs = foldl (\acc x -> if x == n then acc + n else acc) 0 xs

solve1 :: [Int] -> [Int] -> Int
solve1 col1 col2 = sum $ zipWith distance col1 col2

solve2 :: [Int] -> [Int] -> Int
solve2 col1 col2 = foldl (\acc x -> acc + similarityScore x col2) 0 col1

main :: IO ()
main = do
  contents <- readFile "input/day1.txt"
  let listOfLines = lines contents
  let (col1, col2) = unzip $ map splitOnSpace listOfLines
  let answer1 = show $ solve1 (sort col1) (sort col2)
  let answer2 = show $ solve2 col1 col2
  putStrLn $ "Part 1: " ++ answer1
  putStrLn $ "Part 2: " ++ answer2
