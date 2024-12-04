module Main where

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
          countXMASHorizontal xs@(_:xs') ys@(_:ys') zs@(_:zs') = countXMASHorizontal xs' ys' zs' + case (xs, ys, zs) of
            ('M':_:'M':_, _:'A':_:_, 'S':_:'S':_) -> 1 
            ('S':_:'M':_, _:'A':_:_, 'S':_:'M':_) -> 1 
            ('M':_:'S':_, _:'A':_:_, 'M':_:'S':_) -> 1 
            ('S':_:'S':_, _:'A':_:_, 'M':_:'M':_) -> 1 
            (_          , _        , _          ) -> 0
          countXMASHorizontal _ _ _ = 0
            

main :: IO ()
main = getContents >>= print . doThing
