{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.List (group, partition, sort)

type Plot = (Word, Word)

type Edge = (Plot, Plot)

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

getOuterEdges :: [Plot] -> [Edge]
getOuterEdges = concatMap (\case [x] -> [x]; _ -> []) . group . sort . getOuterEdges'
  where
    getOuterEdges' :: [Plot] -> [Edge]
    getOuterEdges' [] = []
    getOuterEdges' ((x, y) : ys) = ((x, y), (x, y + 1)) : ((x, y), (x + 1, y)) : ((x + 1, y), (x + 1, y + 1)) : ((x, y + 1), (x + 1, y + 1)) : getOuterEdges' ys

sameDirection :: Edge -> Edge -> Bool
sameDirection ((x1, _), (x2, _)) ((x3, _), (x4, _)) = (x1 == x2) == (x3 == x4)

mergeEdges :: [Edge] -> [Edge]
mergeEdges [] = []
mergeEdges (e@(x, y) : xs) = case partition ((== y) . fst) xs of
  ([e'@(_, y')], ys) | sameDirection e e' -> mergeEdges $ (x, y') : ys
  _ -> case partition ((== x) . snd) xs of
    ([e'@(x', _)], ys) | sameDirection e e' -> mergeEdges $ (x', y) : ys
    _ -> e : mergeEdges xs

getSides :: [Plot] -> Word
getSides xs = fromIntegral . length . mergeEdges $ getOuterEdges xs

doThing :: String -> String
doThing = show . sum . fmap (cost . snd) . parseInput
  where
    cost :: [Plot] -> Word
    cost xs = fromIntegral (length xs) * getSides xs

main :: IO ()
main = getContents >>= print . doThing
