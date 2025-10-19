module Main where

import Data.Char (isDigit)
import Data.List (foldl', sortBy, tails)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Ord (comparing)

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (p : ps) (x : xs)
  | p == x = isPrefix ps xs
  | otherwise = False

findSubstring :: (Eq a) => [a] -> [a] -> Maybe Int
findSubstring needle haystack =
  case dropWhile (not . isPrefix needle) (tails haystack) of
    [] -> Nothing
    xs -> Just (length haystack - length (head xs))

findEveryInstanceOfSubstring :: (Eq a) => [a] -> [a] -> [Int]
findEveryInstanceOfSubstring needle haystack = go haystack 0
  where
    go [] _ = []
    go xs offset =
      case findSubstring needle xs of
        Just x ->
          let i = offset + x
           in i : go (drop (x + length needle) xs) (i + length needle)
        Nothing -> []

getDigitsPrefix :: String -> String
getDigitsPrefix str = takeWhile isDigit str

getArgsOfLeadingMul :: String -> Maybe (Int, Int)
getArgsOfLeadingMul xs
  | not (isPrefix "mul(" xs) = Nothing
  | not (commaAfterFirstArgument && parenAfterSecondArgument) = Nothing
  | (length firstArgument) > 3 = Nothing
  | (length secondArgument) > 3 = Nothing
  | otherwise = Just (read firstArgument :: Int, read secondArgument :: Int)
  where
    xs' = drop 4 xs
    firstArgument = getDigitsPrefix $ xs'
    commaAfterFirstArgument = ',' == head (drop (length firstArgument) xs')
    secondArgument = getDigitsPrefix $ drop ((length firstArgument) + 1) xs'
    parenAfterSecondArgument = ')' == head (drop ((length firstArgument) + (length secondArgument) + 1) xs')

data InstructionType = Mul Int Int | Do Bool deriving (Show)

getAllMulArgs :: [Int] -> String -> [(Int, InstructionType)]
getAllMulArgs indices input =
  let mulArgs = map (\i -> getArgsOfLeadingMul (drop i input)) indices
      unwrapJust (i, args) =
        if isJust args
          then Just (i, Mul (fst (fromJust args)) (snd (fromJust args)) :: InstructionType)
          else Nothing
   in catMaybes $ map unwrapJust $ zip indices mulArgs

solve :: [InstructionType] -> Int
solve instructions =
  snd $ foldl' step (True, 0) instructions
  where
    step (on, acc) (Mul x y) = (on, if on then acc + x * y else acc)
    step (_, acc) (Do b) = (b, acc)

main :: IO ()
main = do
  input <- readFile "input/day3.txt"
  let mulIndices = findEveryInstanceOfSubstring "mul(" input
  let doIndices = findEveryInstanceOfSubstring "do()" input
  let dontIndices = findEveryInstanceOfSubstring "don't()" input
  let muls = getAllMulArgs mulIndices input
  let dos = zip doIndices $ repeat (Do True :: InstructionType)
  let donts = zip dontIndices $ repeat (Do False :: InstructionType)
  let instructions = map (\(_, instruction) -> instruction) $ sortBy (comparing (\(i, _) -> i)) $ muls ++ dos ++ donts
  let day1Answer =
        solve $
          filter
            ( \instruction ->
                case instruction of
                  Mul _ _ -> True
                  Do _ -> False
            )
            instructions
  let day2Answer = solve instructions
  putStrLn $ "Part 1: " ++ show day1Answer
  putStrLn $ "Part 2: " ++ show day2Answer
