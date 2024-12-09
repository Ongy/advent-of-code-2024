{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Maybe (isJust)

import Debug.Trace (traceShowId)

parseInput :: String -> [Maybe Word]
parseInput = parseInput' True 0
  where
    parseInput' :: Bool -> Word -> String -> [Maybe Word]
    parseInput' _ _ [] = []
    parseInput' True c (x : xs) = replicate (read [x]) (Just c) ++ parseInput' False (c + 1) xs
    parseInput' False c (x : xs) = replicate (read [x]) Nothing ++ parseInput' True c xs

merge :: [Maybe a] -> [Maybe a] -> [Maybe a]
merge xs (Nothing:ys) = merge xs ys
merge (Nothing : xs) (y : ys) = y : merge xs ys
merge (x : xs) ys = x : merge xs ys

compact :: [Maybe a] -> [Maybe a]
compact xs = take (length $ filter isJust xs) $ merge xs (reverse xs)

doThing :: String -> String
doThing = show . sum . zipWith (\x (Just y) -> x * y) [0 ..] . compact . parseInput

main :: IO ()
main = getContents >>= print . doThing
