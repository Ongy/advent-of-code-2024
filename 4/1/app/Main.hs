module Main where
import Data.List (transpose)
import Data.Universe.Helpers (diagonals)

doThing :: String -> String
doThing input = let
    inputLines = lines input
    lineMAS    = sum . fmap countXMAS $ inputLines
    columnMAS  = sum . fmap countXMAS $ transpose inputLines
    diagLMAS   = sum . fmap countXMAS $ diagonals inputLines
    diagCMAS   = sum . fmap countXMAS . diagonals $ fmap reverse inputLines
    in show $ lineMAS + columnMAS + diagLMAS + diagCMAS
    where countXMAS :: String -> Word
          countXMAS [] = 0
          countXMAS ('X':'M':'A':'S':xs) = 1 + countXMAS ('S':xs)
          countXMAS ('S':'A':'M':'X':xs) = 1 + countXMAS ('X':xs)
          countXMAS (_:xs) = countXMAS xs
            

main :: IO ()
main = getContents >>= print . doThing
