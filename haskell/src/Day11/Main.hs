module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

parseInput :: String -> [Int]
parseInput = map (read) . words

countDigits :: Int -> Int
countDigits n
  | n < 10 = 1
  | otherwise = 1 + countDigits (n `div` 10)

leftRightDigits :: Int -> (Int, Int)
leftRightDigits x =
  (x `div` 10^n, x `mod` 10^n)
  where
    n = (countDigits x) `div` 2

blinkStone :: Int -> [Int]
blinkStone stone
  | stone == 0 = [1]
  | even $ countDigits stone =
      let (left, right) = leftRightDigits stone
       in [left, right]
  | otherwise = [stone * 2024]

part1 :: String -> Int
part1 = length . (!! 25) . iterate (>>= blinkStone) . parseInput

updateFreqs :: [Int] -> IntMap Int
updateFreqs = IM.fromListWith (+) . map (,1)

blink :: IntMap Int -> IntMap Int
blink freqMap = IM.unionsWith (+)
  [ (* n) <$> updateFreqs (blinkStone x)
  | (x, n) <- IM.toList freqMap
  ]

-- Uses a frequency map to drastically cut calcuation time. The `sum` works
-- because `IntMap` is foldable; `sum` will sum the values of the `IntMap`.
part2 :: String -> Int
part2 = sum . (!! 75) . iterate blink . updateFreqs . parseInput

main :: IO ()
main = do
  input <- readFile "input/day11.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

