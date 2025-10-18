module Main where

import Data.HashMap (Map)
import qualified Data.HashMap as H
import Data.HashSet (Set)
import qualified Data.HashSet as S
import Data.List (foldl', tails)

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

getAntinodes :: [(Int, Int)] -> Set (Int, Int)
getAntinodes nodes =
  foldl'
    ( \s ((i1, j1), (i2, j2)) ->
        -- INVARIANT: i1 <= i2, because input was parsed left
        let (di, dj) = (i2 - i1, j2 - j1)
         in (i1 - di, j1 - dj) `S.insert` ((i2 + di, j2 + dj) `S.insert` s)
    )
    S.empty
    [(node, node') | (node : rest) <- tails nodes, node' <- rest]

part1 :: Int -> Int -> Map Char [(Int, Int)] -> Int
part1 w h =
  S.size
    . S.filter (\(i, j) -> 0 <= i && i <= h - 1 && 0 <= j && j <= w - 1)
    . H.fold (\nodeList set -> S.union set $ getAntinodes nodeList) S.empty

main :: IO ()
main = do
  input <- readFile "input/day8.txt" 
  let (width, height, nodeMap) = parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 width height nodeMap)
