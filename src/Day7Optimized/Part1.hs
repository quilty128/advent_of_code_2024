{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7Optimized.Part1 where

import Data.List (foldl')

-- This function is not tail recursive due to the `||` operator, but that is not 
-- a problem because it prunes aggressively and the input lists are quite short.
validateEquation :: (Int, [Int]) -> Bool
validateEquation (testVal, (x0 : xs0)) = go x0 xs0
  where
    go acc [] = acc == testVal
    go acc (x : xs)
      | acc > testVal = False
      | otherwise = go (acc + x) xs || go (acc * x) xs

part1 :: [(Int, [Int])] -> Int
part1 =
  foldl'
    ( \acc (testVal, ops) ->
        if validateEquation (testVal, ops)
          then acc + testVal
          else acc
    )
    0
