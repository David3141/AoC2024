module Day04 (part1, part2) where

import Data.Array (Ix (range), bounds)
import MatrixHelpers
  ( Matrix,
    allDirectionsOfLength,
    fromStrings,
    neighbors,
    (!),
  )

part1 :: String -> Int
part1 input = length . filter (== "XMAS") $ allWords
  where
    matrix = prepare input
    allIndices = range . bounds $ matrix
    allWords = concatMap (\coords -> allDirectionsOfLength 4 coords matrix) allIndices

part2 :: String -> Int
part2 input = length allXMas
  where
    matrix = prepare input
    allIndices = range . bounds $ matrix
    allXMas = filter isXMas allIndices

    isXMas :: (Int, Int) -> Bool
    isXMas (m, n)
      | m == 1 = False
      | n == 1 = False
      | element == 'A' = checkCorners
      | otherwise = False
      where
        element = matrix ! (m, n)
        checkCorners = corners == "MSMS" || corners == "MMSS" || corners == "SMSM" || corners == "SSMM"
        corners = case neighbors (m, n) matrix of
          [topLeft, _, topRight, _, _, bottomLeft, _, bottomRight] -> [topLeft, topRight, bottomLeft, bottomRight]
          _ -> []

prepare :: String -> Matrix Char
prepare = fromStrings id . lines
