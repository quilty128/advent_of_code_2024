module Shared.MatrixHelper
  ( chunksOf,
    getRows,
    getCols,
    getDiags,
  )
where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Prelude (Int, div, foldl, map, otherwise, ($), (+), (++), (-), (==))

getRows :: M.Matrix a -> [V.Vector a]
{-# INLINE getRows #-}
getRows matrix =
  map (\i -> M.getRow i matrix) [1 .. M.nrows matrix]

getCols :: M.Matrix a -> [V.Vector a]
{-# INLINE getCols #-}
getCols matrix =
  map (\i -> M.getCol i matrix) [1 .. M.ncols matrix]

getDownDiags :: M.Matrix a -> [V.Vector a]
{-# INLINE getDownDiags #-}
getDownDiags matrix
  | (M.nrows matrix) == 0 = []
  | otherwise =
      (goDown (M.minorMatrix 1 (M.ncols matrix) matrix))
        ++ [M.getDiag matrix]
        ++ (goUp (M.minorMatrix (M.nrows matrix) 1 matrix))
  where
    goUp :: M.Matrix a -> [V.Vector a]
    goUp m
      | (M.nrows m) == 0 = []
      | otherwise = [M.getDiag m] ++ (goUp (M.minorMatrix (M.nrows m) 1 m))
    goDown :: M.Matrix a -> [V.Vector a]
    goDown m
      | (M.nrows m) == 0 = []
      | otherwise = (goDown (M.minorMatrix 1 (M.ncols m) m)) ++ [M.getDiag m]

mirror :: M.Matrix a -> M.Matrix a
{-# INLINE mirror #-}
mirror matrix = foldl (\mat i -> M.switchCols i (cols - i + 1) mat) matrix [1 .. cols `div` 2]
  where
    cols = M.ncols matrix

getDiags :: M.Matrix a -> [V.Vector a]
{-# INLINEABLE getDiags #-}
getDiags matrix =
  let downDiags = getDownDiags matrix
      upDiags = getDownDiags $ mirror $ M.transpose matrix
   in downDiags ++ upDiags

chunksOf :: Int -> Int -> M.Matrix a -> [M.Matrix a]
chunksOf w h matrix =
  [ M.submatrix i (i + h - 1) j (j + w - 1) matrix
  | i <- [1 .. (rows - h + 1)],
    j <- [1 .. (cols - w + 1)]
  ]
  where
    rows = M.nrows matrix
    cols = M.ncols matrix
