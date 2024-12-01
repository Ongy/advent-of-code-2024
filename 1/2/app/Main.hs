module Main where
import Data.List (sort)

main :: IO ()
main = do
    input <- getContents
    let worded = fmap words $ lines input
    let parsed = fmap (fmap (read :: String -> Int)) worded
    let (xs, ys) = unzip $ fmap (\[x, y] -> (x, y)) parsed

    let (xs', ys') = unzip $ fmap (\x -> (x, length $ filter ((==) x) ys)) xs

    print $ sum $ zipWith (*) xs' ys'