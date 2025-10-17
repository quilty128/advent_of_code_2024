{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7Optimized.Part2 where

import Data.List (foldl')

(.||.) :: Int -> Int -> Int
{-# INLINE (.||.) #-}
(.||.) x y = x * (10 ^ digits y) + y
  where
    digits :: Int -> Int
    digits n = ceiling (logBase 10 (fromIntegral n + 1 :: Double))

-- Apply functions left to right
applyFuncs :: [a] -> [a -> a -> a] -> a
applyFuncs [] _ = error "applyFuncs: No operands provided"
applyFuncs (x0 : xs0) funcs0
  | length xs0 /= length funcs0 = error "applyFuncs: Wrong length lists"
  | otherwise = foldl (\acc (f, x) -> f acc x) x0 $ zip funcs0 xs0

-- Maps an Int `ops0` to n operators.
getOperatorList :: Int -> Int -> [Int -> Int -> Int]
getOperatorList n ops0 = take n $ go ops0
  where
    go ops = case ops `mod` 3 of
      0 -> (+) : go (ops `div` 3)
      1 -> (*) : go (ops `div` 3)
      2 -> (.||.) : go (ops `div` 3)

getOperatorLists :: Int -> [[Int -> Int -> Int]]
getOperatorLists len = map (getOperatorList len) [0 .. 3 ^ len - 1]

validateEquation :: (Int, [Int]) -> Bool
validateEquation (testVal, [x]) = testVal == x
validateEquation (testVal, operands) =
  any (\operations -> applyFuncs operands operations == testVal) $
    getOperatorLists (length operands - 1)

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
