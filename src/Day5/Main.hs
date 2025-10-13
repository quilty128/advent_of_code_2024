module Main where

import Data.List (sortBy)
import Data.HashMap (Map)
import qualified Data.HashMap as H

import Shared.Utils (middle, splitOn)

type Page = Int
type Update = [Page]
type Rules = Map Int [Int]

comparePages' :: Rules -> Page -> Page -> Ordering
comparePages' rules pg1 pg2 
  | pg1 == pg2 = EQ
  | otherwise = case H.lookup pg1 rules of
    Just pagesAfter | pg2 `elem` pagesAfter -> LT
    _ -> case H.lookup pg2 rules of
      Just pagesAfter | pg1 `elem` pagesAfter -> GT
      _ -> error $ "No rule for page numbers " ++ (show pg1) ++ " and " ++ (show pg2)

parseRules :: String -> Map Int [Int]
parseRules input =
  let [pageOrderingRules, _] = splitOn "" $ lines input
   in toHashMap $ map (\[a, b] -> (read a, read b)) $ map (splitOn '|') pageOrderingRules
  where
    f hashMap (k, v) = H.insertWith (\newval oldval -> oldval ++ newval) k [v] hashMap
    toHashMap rules = foldl f (H.empty) rules

parseUpdates :: String -> [[Int]]
parseUpdates input =
  let [_, updates] = splitOn "" $ lines input
   in map ((map read) . (splitOn ',')) updates

updateIsCorrect :: Map Int [Int] -> [Int] -> Bool
updateIsCorrect rules update =
  all
    ( \(i, j) ->
        let pg1 = update !! i
            pg2 = update !! j
         in correctlyBefore pg1 pg2
    )
    [ (i, j)
    | i <- [0 .. (length update) - 1],
      j <- [0 .. (length update) - 1],
      i < j
    ]
  where
    -- Ordering rules specify that `pg1` is before `pg2`
    correctlyBefore pg1 pg2 = case (H.lookup pg1 rules, H.lookup pg2 rules) of
      (Just pagesAfter1, Just pagesAfter2) -> pg2 `elem` pagesAfter1 && not (pg1 `elem` pagesAfter2)
      (Just pagesAfter1, Nothing) -> pg2 `elem` pagesAfter1
      (Nothing, Just pagesAfter2) -> not (pg1 `elem` pagesAfter2)
      (Nothing, Nothing) -> error $ "No rule for page " ++ (show pg1)

solve1 :: String -> Int
solve1 input =
  let rules = parseRules input
      correctUpdates = filter (updateIsCorrect rules) $ parseUpdates input
   in sum $ map (\update -> update !! ((length update) `div` 2)) correctUpdates

----- Part 2 -----

solve2 :: String -> Int
solve2 input =
  let rules = parseRules input
      comparePages = comparePages' rules
      incorrectUpdates = filter (not . updateIsCorrect rules) $ parseUpdates input
   in sum $ map (middle) $ map (sortBy comparePages) incorrectUpdates

main :: IO ()
main = do
  input <- readFile "input/day5.txt"
  putStrLn $ "Part 1: " ++ (show $ solve1 input)
  putStrLn $ "Part 2: " ++ (show $ solve2 input)
