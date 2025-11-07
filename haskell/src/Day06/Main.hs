module Main where

import Data.HashSet (Set)
import qualified Data.HashSet as S
import Data.List (nub)

data Direction = North | South | East | West
  deriving (Show, Eq)

----- Helper types -----

type Point = (Int, Int)

(<+>) :: Point -> Point -> Point
{-# INLINE (<+>) #-}
(<+>) (a, b) (c, d) = (a + c, b + d)

toPoint :: Direction -> Point
{-# INLINE toPoint #-}
toPoint dir = case dir of
  North -> (-1, 0)
  South -> (1, 0)
  East -> (0, 1)
  West -> (0, -1)

turnRight :: Direction -> Direction
{-# INLINE turnRight #-}
turnRight dir = case dir of
  North -> East
  East -> South
  South -> West
  West -> North

type Boulders = Set Point

inBounds :: Point -> Point -> Bool
{-# INLINE inBounds #-}
inBounds (iMax, jMax) (i, j) = 0 <= i && i <= iMax && 0 <= j && j <= jMax

----- Solution -----

parseInput :: String -> (Boulders, Point, Point)
parseInput input = (boulders, maxCoord, startPoint)
  where
    grid = lines input
    maxCoord = (length grid - 1, length grid - 1)
    coords = [(i, j) | i <- [0 .. ], j <- [0 .. ], i <= fst maxCoord, j <= snd maxCoord]
    (boulders, startPoint) =
      foldr
        ( \(c, point) (set', startPoint') ->
            case c of
              '#' -> (point `S.insert` set', startPoint')
              '^' -> (set', point)
              _ -> (set', startPoint')
        )
        (S.empty, (0, 0))
        $ zip (concat grid) coords

stepPath :: Boulders -> Point -> Point -> [Point]
stepPath boulders maxCoord p0 = go p0 North []
  where
    go p dir xs
      | p' `inBounds` maxCoord = xs
      | p' `S.member` boulders = go p (turnRight dir) xs
      | otherwise = go p' dir (p' : xs)
      where
        p' = p <+> (toPoint dir)

part1 :: Boulders -> Point -> Point -> Int
part1 boulders maxCoord = length . nub . stepPath boulders maxCoord

-- Uses tortoise and hare
hasLoop :: (Eq a) => [a] -> Bool
hasLoop xs0 = go xs0 (drop 1 xs0)
  where
    go (x : xs) (y : _ : ys) = x == y || go xs ys
    go _ _ = False

-- The confusing parts of this function definition are to ensure that we only test
-- inserting boulders along the guard’s path.
part2 :: Set Point -> Point -> Point -> Int
part2 boulders maxCoord p0 =
  length $ filter goodBoulder $ nub $ stepPath boulders maxCoord p0
  where
    goodBoulder p = p /= p0 && hasLoop (stepPath (S.insert p boulders) maxCoord p0)

main :: IO ()
main = do
  input <- readFile "input/day6.txt"
  let (boulders, maxCoord, startPoint) = parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 boulders maxCoord startPoint)
  putStrLn $ "Part 2: " ++ (show $ part2 boulders maxCoord startPoint)
