module Day03 (part1, part2) where

import Text.Regex.Applicative
    ( RE, Alternative((<|>)), sym, string )
import Text.Regex.Applicative.Common (decimal)
import Data.Foldable (Foldable(foldl'))
import Helpers (fMatchAll)

type Mul = (Int, Int)
data Instruction = MulInst Mul | Do | Dont deriving (Show, Eq)

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . prepare1

part2 :: String -> Int
part2 = sum . map (uncurry (*)) . removeInvalid . prepare2
  where
    removeInvalid :: [Instruction] -> [Mul]
    removeInvalid = snd . foldl' process (True, [])

    process :: (Bool, [Mul]) -> Instruction -> (Bool, [Mul])
    process (_, result) Dont = (False, result)
    process (_, result) Do = (True, result)
    process (isValid, result) (MulInst mulInst)
      | isValid = (isValid, mulInst:result)
      | otherwise = (isValid, result)


prepare1 :: String -> [Mul]
prepare1 = fMatchAll mul

prepare2 :: String -> [Instruction]
prepare2 = fMatchAll (mulInst <|> doInst <|> dontInst)
  where
    mulInst = MulInst <$> mul
    doInst = Do <$ string "do()"
    dontInst = Dont <$ string "don't()"

mul :: RE Char Mul
mul = (,) <$> factor1 <*> factor2
  where
    factor1 = string "mul(" *> decimal <* sym ','
    factor2 = decimal <* sym ')'
