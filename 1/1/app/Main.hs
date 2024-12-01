module Main where
import Data.List (sort)

main :: IO ()
main = do
    input <- getContents
    let worded = fmap words $ lines input
    let parsed = fmap (fmap (read :: String -> Int)) worded
    let (xs, ys) = unzip $ fmap (\[x, y] -> (x, y)) parsed
    let diffed = zipWith (\x y -> abs $ x-y) (sort xs) (sort ys)

    print $ sum diffed