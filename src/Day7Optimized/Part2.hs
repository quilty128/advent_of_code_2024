{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7Optimized.Part2 where

import Data.List (foldl')

(.||.) :: Int -> Int -> Int
{-# INLINE (.||.) #-}
(.||.) x y = x * (10 ^ digits y) + y
  where
    digits :: Int -> Int
    digits n = ceiling (logBase 10 (fromIntegral n + 1 :: Double))

validateEquation :: (Int, [Int]) -> Bool
validateEquation (testVal, (x0 : xs0)) = go x0 xs0
  where
    go acc [] = acc == testVal
    go acc (x : xs)
      | acc > testVal = False
      | otherwise = go (acc + x) xs || go (acc * x) xs || go (acc .||. x) xs

-- The strict fold avoids making a chain of unevaluated thunks
part2 :: [(Int, [Int])] -> Int
part2 =
  foldl'
    ( \acc (testVal, ops) ->
        if validateEquation (testVal, ops)
          then acc + testVal
          else acc
    )
    0
