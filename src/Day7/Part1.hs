-- Incomplete patterns in this file work for all puzzle inputs
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7.Part1 where

import Data.Bits

-- EXPLANATION
--
-- We use an Int (a signed integer of system wordsize) to represent the current
-- permutation of operations when testing all possible combinations, where each
-- zero bit represents (+) and each set bit represents (*). Then we can step
-- through the permutations efficiently using addition. And, for stepping
-- through permutations of n operations, we can stop stepping once the the
-- (n + 1)-th bit becomes set (one-indexed). Finally, we can identify when the
-- (n + 1)-th bit becomes set by using `countTrailingZeros` from Data.Bits.
--
-- For example, to step through the permutations of two operations, to be used
-- for a three operand equation, we want to generate
--   [(+), (+)]
--   [(+), (*)]
--   [(*), (+)]
--   [(*), (*)],
-- and this corresponds to
--   0b00
--   0b01
--   0b10
--   0b11,
-- where each of these integers is the successor of the previous. The successor
-- of 0b11 is 0b100, so we stop stepping at 0b11 and return the list of
-- permutations of operations.
--
-- Then we can simply test if applying any of the permutations to the operands
-- in each line produces the test value to see if an equation is valid. This
-- solves part 1.
--
-- NOTES:
--
-- There is no point using a smaller integer even though we probably need at
-- most 16 bits only 16 bits (in my input the longest "equation" has 12 operands,
-- so 12 bits are needed). In Haskell ever other int type, like Int16, still
-- consumes at least the system’s wordsize.
--
-- Also, there cannot be issues using the Prelude.Int type on systems of very
-- small wordsize; the Haskell language standard guarantees that it has at least
-- the range [-2^29 .. 2^29-1].

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
