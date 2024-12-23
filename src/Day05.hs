module Day05 (part1, part2) where

import Data.List (elemIndex)
import Data.List.Split (splitOn)

type Rule = (Int, Int)
type Update = [Int]

part1 :: String -> Int
part1 input = sum . map middlePage . filter matchesAllRules $ updates
  where
    (rules, updates) = prepare input
    matchesAllRules update = all (`updateMatchesRule` update) rules

part2 :: String -> Int
part2 = const 2 . prepare

prepare :: String -> ([Rule], [Update])
prepare input = case splitOn "\n\n" input of
  [rules, updates] -> (map parseRule (lines rules), map parseUpdate (lines updates))
  _ -> error "Invalid input format"
  where
    parseRule string = case splitOn "|" string of
      [number1, number2] -> (read number1, read number2)
      _ -> error "Invalid rule format"

    parseUpdate = map read . splitOn ","

middlePage :: Update -> Int
middlePage pages = pages !! (length pages `div` 2)

updateMatchesRule :: Rule -> Update -> Bool
updateMatchesRule (p1, p2) pages = case elemIndex p1 pages of
  Just index1 -> case elemIndex p2 pages of
    Just index2 -> index1 < index2
    _ -> True
  _ -> True
