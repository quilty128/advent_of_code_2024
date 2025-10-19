{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7Optimized.Part2 where

(.||.) :: Int -> Int -> Int
{-# INLINE (.||.) #-}
x .||. y = x * (10 ^ digits y) + y
  where
    digits :: Int -> Int
    digits n = ceiling (logBase 10 (fromIntegral n + 1 :: Double))

-- Strangely, using a tuple parameter instead of making this function
-- `Int -> [Int] -> Bool` is almost twice as fast, even with a strict fold in
-- `part2`.
validateEquation :: (Int, [Int]) -> Bool
validateEquation (testVal, (x0 : xs0)) = go x0 xs0
  where
    go acc [] = acc == testVal
    go acc (x : xs)
      | acc > testVal = False
      | otherwise = go (acc + x) xs || go (acc * x) xs || go (acc .||. x) xs

part2 :: [(Int, [Int])] -> Int
part2 = sum . map fst . filter validateEquation
