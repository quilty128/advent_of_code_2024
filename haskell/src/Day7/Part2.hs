{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7.Part2 where

import Data.Bits ((.&.))
import Day7.Part1 (applyFuncs)

(.||.) :: Int -> Int -> Int
{-# INLINE (.||.) #-}
(.||.) x y = read $ show x ++ show y

-- Maps an Int `ops0` to n operators.
getOperators :: Int -> Int -> [Int -> Int -> Int]
getOperators n ops0 = take n $ go ops0
  where
    go ops = case ops `mod` 3 of
      0 -> (+) : go (ops `div` 3)
      1 -> (*) : go (ops `div` 3)
      2 -> (.||.) : go (ops `div` 3)

validateEquation :: (Int, [Int]) -> Bool
validateEquation (testVal, [x]) = testVal == x
validateEquation (testVal, operands) = go 0
  where
    n = length operands - 1
    maxOps = 3 ^ (length operands)
    go ops
      | ops > maxOps = False
      | result == testVal = True
      | otherwise = go (ops + 1)
      where
        result = applyFuncs (getOperators n ops) operands

part2 :: [(Int, [Int])] -> Int
part2 equations = sum $ map fst $ filter validateEquation equations
