module Main where
import Data.List (sort)
import Control.Monad (foldM)

doThing :: String -> String
doThing input = let
    reports = fmap (read :: String -> Int) . words <$> lines input
    safe = filter (any isSafe . dampened) reports

    in show $ length safe
    where isSafe :: [Int] -> Bool
          isSafe [x,y]     = let diff = abs (x - y) in diff >= 1 && diff  <= 3
          isSafe (x:y:z:r) = signum (x - y) == signum (y - z) && isSafe [x, y] && isSafe (y:z:r)

          dampened :: [a] -> [[a]]
          dampened [x]   = [[x], []]
          dampened (x:xs) = let (y:ys) = dampened xs in
            ((x:y): y : fmap (x:) ys )

main :: IO ()
main = getContents >>= print . doThing
