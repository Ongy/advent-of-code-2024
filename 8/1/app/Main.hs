{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map (empty, insertWith, elems)

import Debug.Trace (traceShowId)


parseInput :: String -> (Int, Int, Map Char [(Int, Int)])
parseInput input = let
    inputLines = lines input
    parsed = foldl (\m (lNum, line)  ->
        foldl (\m2 (cNum, c) -> 
            if c == '.' then m2 else Data.Map.insertWith (<>) c [(lNum, cNum)] m2
            ) m $ zip [1 .. ] line
        ) Data.Map.empty $ zip [1 .. ] inputLines
    in (length inputLines,  length (head inputLines), parsed)

findAntis :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
findAntis ((x1, y1), (x2, y2))
    | False <- x1 == x2 && y1 == y2
    = let
    xd = x1 - x2
    yd = y1 - y2
    in [(x1 + xd, y1 + yd), (x2 - xd, y2 - yd)]
findAntis _ = []

doThing :: String -> String
doThing = show . (\(len, width, antennas) -> let
    makePairs :: [a] -> [(a, a)]
    makePairs l = fmap (,) l <*> l
    antis = fmap findAntis . traceShowId . makePairs . traceShowId <$> Data.Map.elems antennas
    in show . length . nub . filter (\(x, y) -> x > 0 && y > 0 && x <= len && y <= width) . concat . concat . traceShowId $ antis
    ) . parseInput 

main :: IO ()
main = getContents >>= print . doThing
