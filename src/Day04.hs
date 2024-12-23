module Day04 (part1, part2) where

import MatrixHelpers (Matrix, allDirectionsOfLength, fromStrings)
import Data.Array (Ix(range), bounds)

part1 :: String -> Int
part1 input = length . filter (== "XMAS") $ allWords
  where
    matrix = prepare input
    allIndices = range . bounds $ matrix
    allWords = concatMap (\coords -> allDirectionsOfLength 4 coords matrix) allIndices

part2 :: String -> Int
part2 = const 2 . prepare

prepare :: String -> Matrix Char
prepare = fromStrings id . lines

-- input :: IO String
-- input = readFile =<< getDataFileName "inputs/day04.txt"
