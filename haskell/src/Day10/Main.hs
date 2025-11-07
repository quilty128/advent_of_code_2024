module Main where

import Data.Char (digitToInt)
import Data.Foldable (foldr')
import Data.HashMap (Map)
import qualified Data.HashMap as H
import Data.HashSet (Set)
import qualified Data.HashSet as S

parseInput :: String -> Map (Int, Int) Int
parseInput input =
  foldr'
    (\(pt, h) topoMap -> H.insert pt h topoMap)
    H.empty
    $ zip points heights
  where
    gridW = length $ head $ lines input
    gridH = length $ lines input
    points = [(i, j) | i <- [0 .. gridW - 1], j <- [0 .. gridH - 1]]
    heights = concatMap (map digitToInt) $ lines input

getTrailheads :: Map (Int, Int) Int -> [(Int, Int)]
getTrailheads = map (fst) . filter ((== 0) . snd) . H.assocs

nextPositions :: Map (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
nextPositions topoMap (i, j) =
  filter (\pos -> topoMap H.! pos == currentHeight + 1) possibleNextPositions
  where
    currentHeight = topoMap H.! (i, j)
    possibleNextPositions =
      [ pos
      | pos <- [(i + 1, j), (i, j - 1), (i - 1, j), (i, j + 1)],
        pos `H.member` topoMap
      ]

peaksFromTrailhead :: Map (Int, Int) Int -> (Int, Int) -> Set (Int, Int)
peaksFromTrailhead topoMap trailheadPos = go trailheadPos
  where
    go pos
      | topoMap H.! pos == 9 = S.singleton pos
      | otherwise =
          foldr'
            (\s1 s2 -> s1 `S.union` s2)
            S.empty
            $ map (go)
            $ nextPositions topoMap pos

scoreTrailhead :: Map (Int, Int) Int -> (Int, Int) -> Int
scoreTrailhead topoMap trailhead = S.size $ peaksFromTrailhead topoMap trailhead

part1 :: String -> Int
part1 input =
  let topoMap = parseInput input
   in sum $ map (scoreTrailhead topoMap) $ getTrailheads topoMap

rateTrailhead :: Map (Int, Int) Int -> (Int, Int) -> Int
rateTrailhead topoMap trailhead = go trailhead
  where
    go pos
      | topoMap H.! pos == 9 = 1
      | otherwise =
          sum $ map (go) $ nextPositions topoMap pos

part2 :: String -> Int
part2 input =
  let topoMap = parseInput input
   in sum $ map (rateTrailhead topoMap) $ getTrailheads topoMap

main :: IO ()
main = do
  input <- readFile "input/day10.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
