module Main where

import Data.List (sortBy)
import Data.HashMap (Map)
import qualified Data.HashMap as H
import Data.HashSet (Set)
import qualified Data.HashSet as HS
import Shared.Utils (middle, splitOn)

type Page = Int

type Update = [Page]

type Rules = Map Page (Set Page)

comparePages' :: Rules -> Page -> Page -> Ordering
comparePages' rules pg1 pg2
  | pg1 == pg2 = EQ
  | otherwise = case H.lookup pg1 rules of
      Just pagesAfter | pg2 `HS.member` pagesAfter -> LT
      _ -> case H.lookup pg2 rules of
        Just pagesAfter | pg1 `HS.member` pagesAfter -> GT
        _ -> error $ "No rule for page numbers " ++ (show pg1) ++ " and " ++ (show pg2)

parseRules :: String -> Rules
parseRules input =
  let [orderingRules, _] = splitOn "" $ lines input
   in toHashMap $ map (\[a, b] -> (read a, read b)) $ map (splitOn '|') orderingRules
  where
    f hashMap (k, v) =
      H.insertWith
        (\newval oldval -> (HS.union newval oldval))
        k
        (HS.singleton v)
        hashMap
    toHashMap rules = foldl f (H.empty) rules

parseUpdates :: String -> [Update]
parseUpdates input =
  let [_, updates] = splitOn "" $ lines input
   in map ((map read) . (splitOn ',')) updates

validateUpdate :: Rules -> Update -> Bool
validateUpdate rules update =
  all
    (uncurry correctlyOrdered)
    [ (update !! i, update !! j)
    | i <- [0 .. (length update) - 1],
      j <- [0 .. (length update) - 1],
      i < j
    ]
  where
    correctlyOrdered pg1 pg2 = comparePages' rules pg1 pg2 /= GT

solve1 :: Rules -> [Update] -> Int
solve1 rules updates =
  let correctUpdates = filter (validateUpdate rules) updates
   in sum $ map (\update -> update !! ((length update) `div` 2)) correctUpdates

solve2 :: Rules -> [Update] -> Int
solve2 rules updates =
  let comparePages = comparePages' rules
      incorrectUpdates = filter (not . validateUpdate rules) updates
   in sum $ map (middle) $ map (sortBy comparePages) incorrectUpdates

main :: IO ()
main = do
  input <- readFile "input/day5.txt"
  let rules = parseRules input
  let updates = parseUpdates input
  putStrLn $ "Part 1: " ++ (show $ solve1 rules updates)
  putStrLn $ "Part 2: " ++ (show $ solve2 rules updates)
