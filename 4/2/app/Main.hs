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
          countXMAS (xs:res@(ys: zs:_)) = countXMAS res + countXMASHorizontal xs ys zs

          countXMASHorizontal :: String -> String -> String -> Word
          countXMASHorizontal [] [] [] = 0
          countXMASHorizontal ('M':xs@(_:'M':_)) (_:ys@('A':_:_)) ('S':zs@(_:'S':_)) = 1 + countXMASHorizontal xs ys zs
          countXMASHorizontal ('S':xs@(_:'M':_)) (_:ys@('A':_:_)) ('S':zs@(_:'M':_)) = 1 + countXMASHorizontal xs ys zs
          countXMASHorizontal ('M':xs@(_:'S':_)) (_:ys@('A':_:_)) ('M':zs@(_:'S':_)) = 1 + countXMASHorizontal xs ys zs
          countXMASHorizontal ('S':xs@(_:'S':_)) (_:ys@('A':_:_)) ('M':zs@(_:'M':_)) = 1 + countXMASHorizontal xs ys zs
          countXMASHorizontal (_:xs) (_:ys) (_:zs) = countXMASHorizontal xs ys zs
            

main :: IO ()
main = getContents >>= print . doThing
