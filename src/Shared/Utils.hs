module Shared.Utils where

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn a list =
  let (part, rest) = break (== a) list
   in part : case rest of
        (x : xs) | x == a -> splitOn a xs
        xs | length xs > 1 -> splitOn a xs
        _ -> []

middle :: [a] -> a
middle xs = 
  if odd len then 
    xs !! (len `div` 2)
  else
    error "middle: length of input is not odd"
  where len = length xs
