module Day02 (part1, part2) where
import Helpers (readIntLists)

part1 :: String -> Int
part1 = length . filter isSafe . prepare

part2 :: String -> Int
part2 = length . filter canBeMadeSafe . prepare
  where
    canBeMadeSafe = any isSafe . candidates

    candidates :: [a] -> [[a]]
    candidates [] = [[]]
    candidates (x:xs) = map (x:) (candidates xs) <> [xs]

prepare :: String -> [[Int]]
prepare = readIntLists " "

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe list = isSafeAsc list || isSafeDesc list
  where
    isSafeAsc [] = True
    isSafeAsc [_] = True
    isSafeAsc (x1:x2:xs) = x1 < x2 && (x2 - x1 <= 3) && isSafeAsc (x2:xs)

    isSafeDesc [] = True
    isSafeDesc [_] = True
    isSafeDesc (x1:x2:xs) = x1 > x2 && (x1 - x2 <= 3) && isSafeDesc (x2:xs)
