{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.List (partition)
import Debug.Trace (traceShowId)

type Plot = (Word, Word)

type Region = (Char, [Plot])

adjacentPlot :: Plot -> Plot -> Bool
adjacentPlot (x, y) (x', y') =
  (x == x' && (y == y' - 1 || y == y' + 1))
    || (y == y' && (x == x' - 1 || x == x' + 1))

parseInput :: String -> [Region]
parseInput input =
  let lineNumbered = zip [0 ..] $ lines input
      numbered = (\(l, xs) -> zipWith (\c v -> (v, (l, c))) [0 ..] xs) <$> lineNumbered
   in buildPlots [] $ concat numbered
  where
    buildPlots :: [Region] -> [(Char, Plot)] -> [Region]
    buildPlots acc [] = acc
    buildPlots acc ((v, plot) : xs) =
      let (merge, retain) = partition (neighboringPlot v plot) acc
          merged = concatMap snd merge
       in buildPlots ((v, plot : merged) : retain) xs
    neighboringPlot :: Char -> Plot -> Region -> Bool
    neighboringPlot v p (v', xs) = v == v' && any (adjacentPlot p) xs

getPerimeter :: [Plot] -> Word
getPerimeter xs = getPerimeter' xs
  where getPerimeter' :: [Plot] -> Word
        getPerimeter' [] = 0
        getPerimeter' (y:ys) = 4 - (fromIntegral . length $ filter (adjacentPlot y) xs) + getPerimeter' ys

doThing :: String -> String
doThing = show . sum . fmap (cost . snd) . parseInput
  where cost :: [Plot] -> Word
        cost xs = fromIntegral (length xs) * getPerimeter xs

main :: IO ()
main = getContents >>= print . doThing
