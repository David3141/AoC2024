module Main where
import System.Environment (getArgs)
import System.TimeIt (timeIt)

import qualified Day01
import Text.Printf (printf)
import Paths_aoc2024 ( getDataFileName )

main :: IO ()
main = do
    daysToRun <- map read <$> getArgs
    mapM_ run daysToRun

run :: Int -> IO ()
run 1 = runPretty 1 (Day01.part1, Day01.part2)
run _ = putStrLn "Not implemented"


runPretty :: (Show a, Show b) => Int -> (String -> a, String -> b) -> IO ()
runPretty day (part1, part2) = do
  let inputFileName = "inputs/day" ++ printf "%02d" day ++ ".txt"
  input <- readFile =<< getDataFileName inputFileName

  putStrLn $ "--- Day " ++ show day ++ " ---"

  putStr "Part 1:     "
  timeIt (print $ part1 input)

  putStr "\nPart 2:     "
  timeIt (print $ part2 input)

  putStrLn ""
