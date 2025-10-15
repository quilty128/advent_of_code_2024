module Main where

import Day7.Part1

parseInput :: String -> [(Int, [Int])]
parseInput input = map parseLine $ lines input
  where
    parseLine line = (read testValue, map read $ words $ drop 1 operandStr)
      where
        (testValue, operandStr) = break (== ':') line

main :: IO ()
main = do
  input <- readFile "input/day7.txt"
  let equations = parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 equations)
