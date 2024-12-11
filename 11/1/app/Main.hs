{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Debug.Trace (traceShowId)
import GHC.Num (integerLogBaseWord)

parseInput :: String -> [Word]
parseInput = fmap read . words

blinkStone :: Word -> [Word]
blinkStone x
  | x == 0 = [1]
  | even (1 + integerLogBaseWord 10 (fromIntegral x)) = let split = 10 ^ (1+ integerLogBaseWord 10 (fromIntegral x) `div` 2) in [x `div` split, x `mod` split]
  | otherwise = [x * 2024]

blink :: [Word] -> [Word]
blink = concatMap blinkStone

doThing :: String -> String
doThing input = show . length $ foldl (flip ($)) (parseInput input) (replicate 25 blink)

main :: IO ()
main = getContents >>= print . doThing
