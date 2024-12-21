module Main where
import System.Environment (getArgs)
import System.TimeIt (timeIt)

import qualified Day01

main :: IO ()
main = do
    daysToRun <- map read <$> getArgs
    mapM_ run daysToRun

run :: Int -> IO ()
run 1 = runPretty 1 (Day01.part1, Day01.part2)
run _ = putStrLn "Not implemented"


runPretty :: (Show a, Show b) => Int -> (IO a, IO b) -> IO ()
runPretty day (part1, part2) = do
  putStrLn $ "--- Day " ++ show day ++ " ---"
  putStr "Part 1:     "
  timeIt (print =<< part1)
  putStr "\nPart 2:     "
  timeIt (print =<< part2)
  putStrLn ""