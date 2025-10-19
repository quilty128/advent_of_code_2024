module Day8.Part2 (part2) where

import Data.HashMap (Map)
import qualified Data.HashMap as H
import Data.HashSet (Set)
import qualified Data.HashSet as S
import Data.List (foldl', tails)

getAntinodes :: Int -> Int -> [(Int, Int)] -> Set (Int, Int)
getAntinodes w h nodes =
  foldl'
    ( \s ((i1, j1), (i2, j2)) ->
        -- INVARIANT: i1 <= i2, because input was parsed left
        let (di, dj) = (i2 - i1, j2 - j1)
            antinodes =
              (takeWhile inGrid $ iterate (\(i, j) -> (i - di, j - dj)) (i1, j1))
                ++ (takeWhile inGrid $ iterate (\(i, j) -> (i + di, j + dj)) (i2, j2))
         in foldl' (\s' antinode -> antinode `S.insert` s') s antinodes
    )
    S.empty
    [(node, node') | (node : rest) <- tails nodes, node' <- rest]
  where
    inGrid (i, j) = 0 <= i && i <= h - 1 && 0 <= j && j <= w - 1

part2 :: Int -> Int -> Map Char [(Int, Int)] -> Int
part2 w h =
  S.size
    . H.fold (\nodeList set -> S.union set $ getAntinodes w h nodeList) S.empty
