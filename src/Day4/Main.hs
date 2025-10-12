module Main where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Shared.MatrixHelper
import Shared.VectorHelper

----- Part 1 -----

isXmas :: V.Vector Char -> Bool
isXmas = (== "XMAS") . V.toList . (V.take 4)

countXmas :: V.Vector Char -> Int
countXmas vec = V.length $ V.filter isXmas $ slices
  where
    slices = V.map (V.take 4) $ V.filter (\v -> (V.length v >= 4)) $ tails vec

solve1 :: String -> Int
solve1 input =
  let matrix = M.fromLists $ lines input
      xmasVectors = (getRows matrix) ++ (getCols matrix) ++ (getDiags matrix)
      reversedXmasVectors = map V.reverse xmasVectors
   in sum $ map countXmas (xmasVectors ++ reversedXmasVectors)

----- Part 2 -----

-- matrix has 3 rows and 3 cols
isXmas2 :: M.Matrix Char -> Bool
isXmas2 matrix = case (M.toLists matrix) of
  [[x11, _, x13], [_, 'A', _], [x31, _, x33]] ->
    case ((x11, x13), (x31, x33)) of
      ( ('M', 'M'),
        ('S', 'S')
        ) -> True
      ( ('M', 'S'),
        ('M', 'S')
        ) -> True
      ( ('S', 'S'),
        ('M', 'M')
        ) -> True
      ( ('S', 'M'),
        ('S', 'M')
        ) -> True
      _ -> False
  [[_, _, _], [_, _, _], [_, _, _]] -> False
  _ -> error "isXmas2: Input matrix must be 3x3"

solve2 :: String -> Int
solve2 input =
  let matrix = M.fromLists $ lines input
      submatrices = chunksOf 3 3 matrix
   in length $ filter isXmas2 submatrices

main :: IO ()
main = do
  input <- readFile "input/day4.txt"
  putStrLn $ "Part 1: " ++ (show $ solve1 input)
  putStrLn $ "Part 2: " ++ (show $ solve2 input)
