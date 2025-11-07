module Main where

import Control.Monad.ST
import Data.Char (digitToInt, isDigit)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

parseInput :: String -> V.Vector Int
parseInput input =
  V.fromList
    $ concatMap
      (\(i, n) -> take n $ repeat (getBlockIdx i))
    $ zip
      [0 .. length input - 1]
      input'
  where
    input' = map digitToInt $ filter isDigit input
    getBlockIdx idx =
      if idx `mod` 2 == 0
        then idx `div` 2
        else -1

moveBlocks :: V.Vector Int -> V.Vector Int
moveBlocks disk = runST $ do
  mDisk <- V.thaw disk
  go mDisk 0 (MV.length mDisk - 1)
  where
    go mDisk i j
      | i >= j = V.unsafeFreeze mDisk
      | otherwise = do
          valI <- MV.unsafeRead mDisk i
          valJ <- MV.unsafeRead mDisk j
          case (valI, valJ) of
            (_, -1) -> go mDisk i (j - 1)
            (-1, _) -> do
              MV.unsafeWrite mDisk i valJ
              MV.unsafeWrite mDisk j (-1)
              go mDisk (i + 1) (j - 1)
            _ -> go mDisk (i + 1) j

data Slice = Slice
  { index :: Int,
    len :: Int
  }
  deriving (Show, Eq)

-- `fst` in return value is the empty slices and `snd` is the file slices
getSlices :: Vector Int -> (Vector Slice, Vector Slice)
getSlices disk =
  V.partition (isEmpty . index) $
    V.fromList $
      map (\(i, l) -> Slice {index = i, len = l}) $
        zip sliceIndices sliceLengths
  where
    isSliceIndex i = i == 0 || (disk V.! i /= disk V.! (i - 1))
    isEmpty i = disk V.! i == -1
    sliceIndices = filter isSliceIndex [0 .. V.length disk - 1]
    sliceLengths = map (uncurry (-)) $ zip (drop 1 sliceIndices) sliceIndices

-- Moves file; returns the new slice data of the destination slice
moveFile :: MVector s Int -> Slice -> Slice -> ST s Slice
moveFile mDisk (Slice dstIndex dstLen) (Slice srcIndex srcLen) = do
  let dstSlice = MV.slice dstIndex srcLen mDisk
  let srcSlice = MV.slice srcIndex srcLen mDisk
  MV.copy dstSlice srcSlice
  MV.set srcSlice (-1)
  let newIndex = dstIndex + srcLen
  let newLen = dstLen - srcLen
  return Slice {index = newIndex, len = newLen}

moveFiles :: MVector s Int -> ST s ()
moveFiles mDisk = do
  frozenDisk <- V.unsafeFreeze mDisk
  let (emptySlices0, fileSlices0) = getSlices frozenDisk
  mEmptySlices0 <- V.unsafeThaw emptySlices0
  let findEmptySlice (Slice srcIdx srcLen) =
        V.find (\(_, Slice _ slcLen) -> srcLen <= slcLen) $
          V.takeWhile (\(_, Slice slcIdx _) -> slcIdx < srcIdx) $
            V.indexed emptySlices0
  let go mEmptySlices fileSlices
        | V.length fileSlices == 0 = return ()
        | otherwise = do
            let src = V.head fileSlices
            case findEmptySlice src of
              Just (i, dst) ->
                moveFile mDisk dst src >>= MV.write mEmptySlices i
              Nothing -> return ()
            go mEmptySlices (V.tail fileSlices)
  go mEmptySlices0 $ V.reverse fileSlices0

calculateChecksum :: V.Vector Int -> Int
calculateChecksum disk =
  V.sum $
    V.imap
      (\i blockId -> if blockId /= -1 then i * blockId else 0)
      disk

part1 :: String -> Int
part1 = calculateChecksum . moveBlocks . parseInput

part2 :: String -> Int
part2 input =
  runST $
    V.unsafeThaw (parseInput input) >>= \mDisk ->
      moveFiles mDisk >> V.unsafeFreeze mDisk >>= return . calculateChecksum

main :: IO ()
main = do
  input <- readFile "input/day9.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
