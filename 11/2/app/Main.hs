{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Debug.Trace (traceShowId)
import GHC.Num (integerLogBaseWord)
import Data.Function.Memoize (memoize2)

parseInput :: String -> [Int]
parseInput = fmap read . words

blinkMemoize :: Int -> Int -> Int
blinkMemoize = memoize2 blinkedCount

blinkedCount :: Int -> Int -> Int
blinkedCount _ 0 = 1
blinkedCount 0 c = blinkMemoize 1 (c - 1)
blinkedCount x c = let
  numDigits = 1 + integerLogBaseWord 10 (fromIntegral x)
  split = 10 ^ (numDigits `div` 2)
  nextBlinks = c - 1
  in if even numDigits
    then blinkMemoize (x `div` split) nextBlinks + blinkMemoize (x `mod` split) nextBlinks
    else blinkMemoize (x * 2024) nextBlinks

doThing :: String -> String
doThing = show . sum . fmap (`blinkedCount` 75) . parseInput

main :: IO ()
main = getContents >>= print . doThing
