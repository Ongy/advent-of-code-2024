module Main where
import Data.List (sort)

doThing :: String -> String
doThing input = let
    reports = fmap (read :: String -> Int) . words <$> lines input
    safe = filter isSafe reports

    in show $ length safe
    where isSafe [x,y] = let diff = abs (x - y) in diff >= 1 && diff  <= 3
          isSafe (x:y:z:r) = signum (x - y) == signum (y - z) && isSafe [x, y] && isSafe (y:z:r)

main :: IO ()
main = getContents >>= print . doThing
