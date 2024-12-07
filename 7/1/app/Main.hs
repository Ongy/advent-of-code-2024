{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import Data.Char (isDigit)

data TestLine = TestLine {
    testValue :: Word,
    testInputs :: [Word]
    } deriving (Show)

operators :: Num a => [a -> a -> a]
operators = [(*), (+)]

parseTestLine :: String -> TestLine
parseTestLine line =
    TestLine (read $ takeWhile isDigit line) (fmap read . words . drop 1 . dropWhile isDigit $ line )

buildPossibles :: Num a => [a -> a -> a] -> [a] -> [a]
buildPossibles _ [] = []
buildPossibles o (x:xs) = buildPossibles' [x] o xs
    where
    buildPossibles' :: Num a=> [a] -> [a -> a -> a] -> [a] -> [a]
    buildPossibles' acc _ [] = acc
    buildPossibles' acc ops (y:ys) = let
        applications = fmap ($ y) ops
        in buildPossibles' (applications <*> acc) ops ys

doThing :: String -> String
doThing input = let
    testLines = parseTestLine <$> lines input
    in show $ sum . fmap testValue $ filter (\(TestLine v i) -> v `elem` buildPossibles operators i) testLines

main :: IO ()
main = getContents >>= print . doThing
