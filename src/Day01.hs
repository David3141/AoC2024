module Day01 (part1, part2) where

import Data.List (sort, transpose)
import Helpers (readStrings)

part1 :: IO Int
part1 = sum . map abs . uncurry (zipWith (-)) <$> readLists

part2 :: IO Int
part2 = return 2000

readLists :: IO ([Int], [Int])
readLists = do
  [listA, listB] <-
    map (sort . map read)
      . transpose
      . map words
      <$> readStrings "inputs/day01.txt"

  return (listA, listB)
  