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

findAntis :: Int -> Int -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
findAntis len width ((x1, y1), (x2, y2))
    | False <- x1 == x2 && y1 == y2
    = let
    xd = x1 - x2
    yd = y1 - y2
    filterFun = takeWhile (\(x, y) -> x > 0 && y > 0 && x <= len && y <= width)
    genLeft m = (x1 + xd * m, y1 + yd * m) : genLeft (m + 1)
    genRight m = (x2 - xd * m, y2 - yd * m) : genRight (m + 1)
    in filterFun (genLeft 0) ++ filterFun (genRight 0)
findAntis _ _ _ = []

doThing :: String -> String
doThing = show . (\(len, width, antennas) -> let
    makePairs :: [a] -> [(a, a)]
    makePairs l = fmap (,) l <*> l
    antis = fmap (findAntis len width) . traceShowId . makePairs . traceShowId <$> Data.Map.elems antennas
    in show . length . nub . concat . concat . traceShowId $ antis
    ) . parseInput 

main :: IO ()
main = getContents >>= print . doThing
