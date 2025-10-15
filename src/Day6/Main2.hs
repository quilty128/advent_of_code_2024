{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad ((>=>))
import Data.HashSet (Set)
import qualified Data.HashSet as S
import Data.List (last, nub, unfoldr)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Maybe (isJust, mapMaybe)
import Prelude hiding (Left, Right)

----- Helper stuff -----

type LabMap = Matrix Char

type Position = (Int, Int)

(<+>) :: Position -> Position -> Position
(<+>) (a, b) (c, d) = (a + c, b + d)

data Direction = Up | Right | Down | Left deriving (Enum, Show, Eq)

directionToTuple :: Direction -> (Int, Int)
{-# INLINE directionToTuple #-}
directionToTuple dir =
  case dir of
    Up -> (-1, 0)
    Right -> (0, 1)
    Down -> (1, 0)
    Left -> (0, -1)

directionFromChar :: Char -> Maybe Direction
{-# INLINE directionFromChar #-}
directionFromChar char =
  case char of
    '^' -> Just Up
    '>' -> Just Right
    'v' -> Just Down
    '<' -> Just Left
    _ -> Nothing

turnRight :: Direction -> Direction
{-# INLINE turnRight #-}
turnRight Left = Up
turnRight dir = succ dir

nextPosition :: Position -> Direction -> Position
{-# INLINE nextPosition #-}
nextPosition pos dir = pos <+> (directionToTuple dir)

data Guard = Guard
  { position :: Position,
    direction :: Direction
  }
  deriving (Show)

guardFromChar :: Position -> Char -> Maybe Guard
{-# INLINE guardFromChar #-}
guardFromChar pos char =
  fmap (\dir -> Guard pos dir) (directionFromChar char)

getGuard :: LabMap -> Guard
getGuard labMap = guard
  where
    (guard : _) =
      mapMaybe
        (\pos -> guardFromChar pos $ labMap M.! pos)
        [ (i, j)
        | i <- [1 .. M.ncols labMap],
          j <- [1 .. M.nrows labMap]
        ]

----- Solution -----

parseInput :: String -> (LabMap, Guard)
parseInput input = (normalizedLabMap, (Guard pos dir))
  where
    labMap = M.fromLists $ lines input
    (Guard pos dir) = getGuard labMap
    normalizedLabMap = M.unsafeSet '.' pos labMap

-- Returns `Nothing` if step takes Guard out of the map
step :: LabMap -> Guard -> Maybe Guard
{-# INLINE step #-}
step labMap guard = go guard
  where
    go :: Guard -> Maybe Guard
    go (Guard (i, j) dir) =
      let (iNext, jNext) = nextPosition (i, j) dir
       in case M.safeGet iNext jNext labMap of
            Just c | c == '#' -> go (Guard (i, j) (turnRight dir))
            Just _ -> Just (Guard (iNext, jNext) dir)
            Nothing -> Nothing

getGuardPath :: LabMap -> Guard -> [Position]
getGuardPath labMap guard =
  let startPos = case guard of Guard pos _ -> pos
   in startPos : unfoldr (step labMap >=> \(Guard pos dir) -> pure (pos, Guard pos dir)) guard

solve1 :: LabMap -> Guard -> Int
solve1 labMap guard = length $ nub $ getGuardPath labMap guard

----- Part 2 -----

data NewObstacle = Opt1 | Opt2 | Opt3 | Opt4 | Opt5 | Opt6

-- Returns `Nothing` if not in grid
validPosition :: LabMap -> Position -> Bool
validPosition labMap (i, j) =
  case M.safeGet i j labMap of
    Just c -> c == '#'
    Nothing -> False

adjacentPositions :: LabMap -> Position -> [Position]
adjacentPositions labMap (i, j) =
  filter
    (inGrid)
    [ (i, j) <+> directionToTuple dir | dir <- [Up, Right, Down, Left] ]
  where
    inGrid (i, j) = isJust $ M.safeGet i j labMap

-- Probably Done
possibleObstaclePositions :: LabMap -> Guard -> NewObstacle -> Set Position
possibleObstaclePositions labMap (Guard pos dir) Opt1 =
  S.fromList $
  filter
    (validPosition labMap)
    [ pos <+> directionToTuple dir'
    | dir' <- [Up, Right, Down, Left],
      dir' /= dir
    ]
possibleObstaclePositions labMap _ Opt2 =
  S.fromList $
  [ (i, j) 
  | i <- [ 1 .. (M.ncols labMap) `div` 2],
    j <- [ 1 .. (M.nrows labMap) `div` 2]
  ]
-- WTF!! The specifications are so imprecise
possibleObstaclePositions labMap _ Opt3 = S.empty
possibleObstaclePositions labMap _ Opt4 =
  if validPosition labMap (i, 2)
    then
      S.singleton (i, 2)
    else
      S.empty
  where
    i = M.nrows labMap - 1
possibleObstaclePositions labMap _ Opt5 =
  if validPosition labMap (i, 4)
    then
      S.singleton (i, 4)
    else
      S.empty
  where
    i = M.nrows labMap - 1
-- Probably Done
possibleObstaclePositions labMap guard Opt6 =
  let lastSquare = last $ getGuardPath labMap guard
      universalSolventPos = head $ filter (not . (validPosition labMap)) $ 
        adjacentPositions labMap lastSquare 
   in S.fromList $ adjacentPositions labMap universalSolventPos

-- solve2 :: LabMap -> Guard -> Int
-- solve2 labMap guard = unimplemented

main :: IO ()
main = do
  input <- readFile "input/day6.txt"
  let (labMap, guard) = parseInput input
  putStrLn $ "Part 1: " ++ (show $ solve1 labMap guard)
