module Day01 (part1, part2, prepare) where

import Data.List (nub, sort, transpose)

part1 :: String -> Int
part1 = sum . map abs . uncurry (zipWith (-)) . prepare

part2 :: String -> Int
part2 input = sum . map countInBothAndMultiply $ nub listA
  where
    (listA, listB) = prepare input

    countInBothAndMultiply :: Int -> Int
    countInBothAndMultiply x = x * countElemInSortedList x listA * countElemInSortedList x listB

countElemInSortedList :: (Ord a) => a -> [a] -> Int
countElemInSortedList element list = count' element list 0
  where
    count' :: (Ord a) => a -> [a] -> Int -> Int
    count' _ [] currentResult = currentResult
    count' x (y : ys) currentResult
      | x > y = count' x ys currentResult
      | x == y = count' x ys (currentResult + 1)
      | otherwise = currentResult

prepare :: String -> ([Int], [Int])
prepare input = case processed of
  [listA, listB] -> (listA, listB)
  _ -> error "Unexpected input format"
  where
    processed =
      map (sort . map read)
        . transpose
        . map words
        . lines
        $ input
