{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Maybe (isJust, fromMaybe)

import Debug.Trace (traceShowId)
import qualified Data.Bifunctor

parseInput :: String -> [(Word, Maybe Word)]
parseInput = parseInput' True 0
  where
    parseInput' :: Bool -> Word -> String -> [(Word, Maybe Word)]
    parseInput' _ _ [] = []
    parseInput' True c (x : xs) = (read [x], Just c) : parseInput' False (c + 1) xs
    parseInput' False c (x : xs) = (read [x], Nothing) : parseInput' True c xs

compact :: [(Word, Maybe Word)] -> [(Word, Maybe Word)]
compact [] = []
compact xs = case reverse xs of 
  (y@(_, Nothing):ys) -> compact (reverse ys) ++ [y]
  (y@(len, _):ys) -> let 
    tryMove :: [(Word, Maybe Word)] -> ([(Word, Maybe Word)], Bool)
    tryMove [] = ([], False)
    tryMove (z@(_, Just _):zs) = Data.Bifunctor.first (z:) $ tryMove zs
    tryMove ((len2, Nothing):zs) | len == len2 = (y:zs, True)
    tryMove ((len2, Nothing):zs) | len < len2 = (y:(len2 - len, Nothing):zs, True)
    tryMove (z:zs) = Data.Bifunctor.first (z:) $ tryMove zs
    (zs, found) = tryMove $ reverse ys
    in compact zs ++ if found then [(len, Nothing)] else [y | not found]

expand :: [(Word, Maybe a)] -> [Maybe a]
expand [] = []
expand ((len, x):xs) = replicate (fromIntegral len) x ++ expand xs

doThing :: String -> String
doThing = show . sum . zipWith (*) [0..] . map (fromMaybe 0) . expand . compact . parseInput

main :: IO ()
main = getContents >>= print . doThing
