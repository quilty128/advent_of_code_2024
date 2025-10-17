{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7Optimized.Part1 where

import Data.Bits (shiftR, (.&.))
import Data.List (foldl')

-- Apply functions left to right
applyFuncs :: [a] -> [a -> a -> a] -> a
applyFuncs [] _ = error "applyFuncs: No operands provided"
applyFuncs (x0 : xs0) funcs0
  | length xs0 /= length funcs0 = error "applyFuncs: Wrong length lists"
  | otherwise = foldl (\acc (f, x) -> f acc x) x0 $ zip funcs0 xs0

getOperatorList :: Int -> Int -> [(Int -> Int -> Int)]
{-# INLINE getOperatorList #-}
getOperatorList n ops0 = take n $ step ops0
  where
    step ops = case ops .&. 1 of
      0 -> (+) : step (ops `shiftR` 1)
      1 -> (*) : step (ops `shiftR` 1)

getOperatorLists :: Int -> [[Int -> Int -> Int]]
getOperatorLists len = map (getOperatorList len) [0 .. 3 ^ len - 1]

validateEquation :: (Int, [Int]) -> Bool
validateEquation (testVal, [x]) = testVal == x
validateEquation (testVal, operands) =
  any (\operations -> applyFuncs operands operations == testVal) $
    getOperatorLists (length operands - 1)

part1 :: [(Int, [Int])] -> Int
part1 =
  foldl'
    ( \acc (testVal, ops) ->
        if validateEquation (testVal, ops)
          then acc + testVal
          else acc
    )
    0
