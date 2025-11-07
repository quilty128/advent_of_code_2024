module Main where

import Data.HashMap (Map)
import qualified Data.HashMap as H
import Data.List (foldl')
import Day8.Part1
import Day8.Part2

parseInput :: String -> (Int, Int, Map Char [(Int, Int)])
parseInput input =
  ( width,
    height,
    foldl'
      ( \hashMap (c, pos) -> case c `H.lookup` hashMap of
          Just xs -> H.insert c (pos : xs) hashMap
          Nothing -> H.insert c [pos] hashMap
      )
      H.empty
      $ filter (\(c, _) -> c /= '.')
      $ zip
        [c | c <- input, c /= '\n']
        [(i, j) | i <- [0 .. height - 1], j <- [0 .. width - 1]]
  )
  where
    inputLines = lines input
    width = length $ head inputLines
    height = length inputLines

main :: IO ()
main = do
  input <- readFile "input/day8.txt"
  let (width, height, nodeMap) = parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 width height nodeMap)
  putStrLn $ "Part 2: " ++ (show $ part2 width height nodeMap)
