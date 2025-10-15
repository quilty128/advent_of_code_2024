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

data Guard = Guard Point Direction
  deriving (Show, Eq)

guardPos :: Guard -> Point
{-# INLINE guardPos #-}
guardPos (Guard pos _) = pos

inBounds :: Point -> Guard -> Bool
{-# INLINE inBounds #-}
inBounds (iMax, jMax) (Guard (i, j) _) =
  0 <= i && i <= iMax && 0 <= j && j <= jMax

----- Solution -----

parseInput :: String -> (Boulders, Point, Guard)
parseInput input = (boulders, (width - 1, height - 1), guard)
  where
    grid = lines input
    width = case grid of (firstRow : _) -> length firstRow
    height = length grid
    (boulders, guard) =
      foldr
        ( \(c, point) (set', guard') ->
            case c of
              '#' -> (point `S.insert` set', guard')
              '^' -> (set', Guard point North)
              'v' -> (set', Guard point South)
              '>' -> (set', Guard point East)
              '<' -> (set', Guard point West)
              _ -> (set', guard')
        )
        (S.empty, Guard (0, 0) North)
        $ zip (concat grid) [(i, j) | i <- [0 .. height - 1], j <- [0 .. width - 1]]

stepPath :: Boulders -> Point -> Guard -> [Guard]
stepPath boulders maxCoord guard = takeWhile (inBounds maxCoord) $ iterate (stepOnce) guard
  where
    stepOnce (Guard pos dir)
      | pos' `S.member` boulders = Guard pos (turnRight dir)
      | otherwise = Guard pos' dir
      where
        pos' = pos <+> (toPoint dir)

part1 :: Boulders -> Point -> Guard -> Int
part1 boulders maxCoord =
  length . nub . map guardPos . stepPath boulders maxCoord

-- Uses tortoise and hare
hasLoop :: (Eq a) => [a] -> Bool
hasLoop xs0 = go xs0 (drop 1 xs0)
  where
    go (x : xs) (y : _ : ys) = x == y || go xs ys
    go _ _ = False

-- The confusing parts of this function definition are to ensure that we only test
-- inserting boulders along the guard’s path.
part2 :: Set Point -> Point -> Guard -> Int
part2 boulders maxCoord guard =
  length $ filter goodBoulder $ nub . map guardPos $ stepPath boulders maxCoord guard
  where
    goodBoulder p = p /= (guardPos guard) && hasLoop (stepPath (S.insert p boulders) maxCoord guard)

main :: IO ()
main = do
  input <- readFile "input/day6.txt"
  let (boulders, maxCoord, guard) = parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 boulders maxCoord guard)
  putStrLn $ "Part 2: " ++ (show $ part2 boulders maxCoord guard)
