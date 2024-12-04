module Main where
import Data.List (transpose)

doThing :: String -> String
doThing input = let
    inputLines = lines input
    in show $ countXMAS inputLines
    where countXMAS :: [String] -> Word
          countXMAS [] = 0
          countXMAS [_] = 0
          countXMAS [_, _] = 0
          countXMAS (xs: ys: zs:res) = countXMAS (ys:zs:res) + countXMASHorizontal xs ys zs

          countXMASHorizontal :: String -> String -> String -> Word
          countXMASHorizontal [] [] [] = 0
          countXMASHorizontal ('M':x:'M':xs) (_:'A':y:ys) ('S':z:'S':zs) = 1 + countXMASHorizontal (x:'M':xs) ('A':y:ys) (z:'S':zs)
          countXMASHorizontal ('S':x:'M':xs) (_:'A':y:ys) ('S':z:'M':zs) = 1 + countXMASHorizontal (x:'M':xs) ('A':y:ys) (z:'M':zs)
          countXMASHorizontal ('M':x:'S':xs) (_:'A':y:ys) ('M':z:'S':zs) = 1 + countXMASHorizontal (x:'S':xs) ('A':y:ys) (z:'S':zs)
          countXMASHorizontal ('S':x:'S':xs) (_:'A':y:ys) ('M':z:'M':zs) = 1 + countXMASHorizontal (x:'S':xs) ('A':y:ys) (z:'M':zs)
          countXMASHorizontal (_:xs) (_:ys) (_:zs) = countXMASHorizontal xs ys zs
            

main :: IO ()
main = getContents >>= print . doThing
