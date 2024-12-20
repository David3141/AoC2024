module Main where
import System.Environment (getArgs)

main :: IO ()
main = do
    daysToRun <- map read <$> getArgs
    mapM_ run daysToRun

run :: Int -> IO ()
run = print
