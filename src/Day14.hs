module Day14 (part1, part2) where

part1 :: String -> Int
part1 = const 1 . prepare

part2 :: String -> Int
part2 = const 2 . prepare

prepare :: String -> String
prepare = id
