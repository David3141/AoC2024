module Day01 (part1, part2) where

import Data.List (sort, transpose, nub)
import Helpers (readStrings)

part1 :: IO Int
part1 = sum . map abs . uncurry (zipWith (-)) <$> readLists

part2 :: IO Int
part2 = do
  (listA, listB) <- readLists

  let countInBothAndMultiply x = x * countElemInSortedList x listA * countElemInSortedList x listB

  return . sum . map countInBothAndMultiply $ nub listA

countElemInSortedList :: Ord a => a -> [a] -> Int
countElemInSortedList element list = count' element list 0
  where
    count' :: Ord a => a -> [a] -> Int -> Int
    count' _ [] currentResult = currentResult
    count' x (y:ys) currentResult
      | x > y = count' x ys currentResult
      | x == y = count' x ys (currentResult + 1)
      | otherwise = currentResult

readLists :: IO ([Int], [Int])
readLists = do
  [listA, listB] <-
    map (sort . map read)
      . transpose
      . map words
      <$> readStrings "inputs/day01.txt"

  return (listA, listB)
