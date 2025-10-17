-- Incomplete patterns in this file work for all puzzle inputs
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7.Part1 where

import Data.Bits

getFuncs :: Int -> Int -> [(Int -> Int -> Int)]
{-# INLINE getFuncs #-}
getFuncs n ops0 = take n $ step ops0
  where
    step ops = case ops .&. 1 of
      0 -> (+) : step (ops `shiftR` 1)
      1 -> (*) : step (ops `shiftR` 1)

-- Apply binary operations left to right; `length xs0` must be one greater than
-- `length funcs0`.
applyFuncs :: [a -> a -> a] -> [a] -> a
applyFuncs _ [] = error "applyFuncs: No operands provided"
applyFuncs funcs0 (x0 : xs0) =
  foldl (\acc (f, x) -> f acc x) x0 $ zip funcs0 xs0

validateEquation :: (Int, [Int]) -> Bool
validateEquation (testVal, operands) = go 0
  where
    n = length operands - 1
    go ops
      | result == testVal = True
      | popCount ops >= n = False
      | otherwise = go (ops + 1)
      where
        result = applyFuncs (getFuncs n ops) operands

part1 :: [(Int, [Int])] -> Int
part1 equations = sum $ map fst $ filter validateEquation equations
