{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import Data.Char (isDigit, isSpace)
import GHC.Num (integerLogBase)

data TestLine = TestLine {
    testValue :: Word,
    testInputs :: [Word]
    } deriving (Show)

concatOp :: Integral a => a -> a -> a
concatOp a b = a * (10 ^ (integerLogBase 10 (toInteger b) + 1)) + b

concatOpShow :: (Show a, Read a) => a -> a -> a
concatOpShow a b = read $ show a ++ show b

-- For quickech at some point
propEqual :: Word -> Word -> Bool
propEqual a b = concatOp a b == concatOpShow a b

operators :: (Integral a, Show a, Read a) => [a -> a -> a]
operators = [(*), (+), flip concatOp]


parseTestLine :: String -> TestLine
parseTestLine line =
    TestLine (read $ takeWhile isDigit line) (fmap read . words . dropWhile (not . isSpace) $ line )

buildPossibles :: Num a => [a -> a -> a] -> [a] -> [a]
buildPossibles _ [] = []
buildPossibles o (x:xs) = buildPossibles' [x] o xs
    where
    buildPossibles' :: Num a => [a] -> [a -> a -> a] -> [a] -> [a]
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
