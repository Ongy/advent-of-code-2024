module Main where
import Data.List (sort)
import Data.Maybe (maybeToList)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Debug.Trace (trace, traceId, traceShowId)

doThing :: String -> String
doThing =
    show . sum . uncurry (zipWith (*)) . unzip . findMuls 
    where findMuls :: String -> [(Int, Int)]
          findMuls ('m':'u':'l':'(':xs) = maybeToList (tryReadMul xs) ++ findMuls xs
          findMuls (_:xs) = findMuls xs
          findMuls [] = []

          tryReadMul :: String -> Maybe (Int, Int)
          tryReadMul xs = let
            first = takeWhile isDigit xs
            in case dropWhile isDigit xs of
                (',':ys) -> let second = takeWhile isDigit ys in
                    case dropWhile isDigit ys of 
                        (')':_) -> (,) <$> readMaybe first <*> readMaybe second
                        _ -> Nothing
                _ -> Nothing

            

main :: IO ()
main = getContents >>= print . doThing
