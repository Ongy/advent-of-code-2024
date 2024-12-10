{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map (empty, insertWith, lookup)
import Data.Set (Set)
import qualified Data.Set (union, singleton, toList)

import Debug.Trace (traceShowId)
import Data.Maybe (maybe)

type Spot = (Word, Word)
type Trail = [(Word, Word)]
type ElevationMap = Map Word (Set Spot)

parseInput :: String -> ElevationMap
parseInput = foldl parseLine Data.Map.empty . zip [0 ..] . lines
  where
    parseLine :: ElevationMap -> (Word, String) -> ElevationMap
    parseLine m (l, line) = foldl (\m1 (c, v) ->
      Data.Map.insertWith Data.Set.union (read [v]) (Data.Set.singleton (l, c)) m1
      ) m . zip [0 ..] $ line

nextTo :: Spot -> Spot -> Bool
nextTo (x, y) (x', y') =
  (x == x' && (y == y' + 1 || y == y' - 1)) ||
  (y == y' && (x == x' + 1 || x == x' - 1))

nextSteps :: ElevationMap -> Word -> Spot -> [Spot]
nextSteps m s p = filter (nextTo p) . maybe [] Data.Set.toList $ Data.Map.lookup s m

allSteps :: ElevationMap -> Word -> Spot -> [Spot]
allSteps _ 9 p = [p]
allSteps m s p = let next = s + 1 in case nextSteps m next p of
  [] -> []
  xs -> concatMap (allSteps m next) xs

findTrails :: ElevationMap -> [Spot]
findTrails m = case Data.Map.lookup 0 m of
  Nothing -> []
  Just zeroes -> concatMap (nub . allSteps m 0) zeroes

doThing :: String -> String
doThing = show . sum . fmap length . findTrails . parseInput

main :: IO ()
main = getContents >>= print . doThing
