{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, lookup, assocs, insertWith, empty)
import Data.Set (Set, singleton, union, member)
import qualified Data.Bifunctor
import Debug.Trace (traceShowId)
import Data.List (sortBy)

buildPrintOrder :: [(Word, Word)] -> Word -> Word -> Ordering
buildPrintOrder constraints = \left right -> let
    checkGT = case Data.Map.lookup right ordMap of
        Just s -> if Data.Set.member left s then GT else EQ
        _ -> EQ
    in case Data.Map.lookup left ordMap of
    Just s -> if Data.Set.member right s then LT else checkGT
    _ -> checkGT
    where ordMap ::  Map Word (Set Word)
          ordMap =  initMap
          initMap :: Map Word (Set Word)
          initMap = foldr (\(k, v) m -> Data.Map.insertWith Data.Set.union k (Data.Set.singleton v) m) Data.Map.empty constraints

doThing :: String -> String
doThing input = let
    inputLines = lines input
    orderConstraints = (\l -> (readWord (takeWhile isDigit l), readWord (drop 1 $ dropWhile (/= '|') l))) <$> takeWhile (not . null) inputLines
    printOrders = fmap readWord . splitOn "," <$> filter (not . null) (dropWhile (not . null) inputLines)
    order = buildPrintOrder orderConstraints
    isOrdered :: [Word] -> Bool
    isOrdered (x:ys) = isOrdered ys && all (\y -> case order x y of
        GT -> False
        _ -> True) ys
    isOrdered [_] = True
    isOrdered _ = True
    unordered = filter (not . isOrdered) printOrders
    in show . sum . traceShowId  . fmap middle $ map (sortBy order) unordered
    where readWord :: String -> Word
          readWord = read
          middle :: [a] -> a
          middle l@(_:_:_:_) = middle $ tail $ init l
          middle [l]         = l




main :: IO ()
main = getContents >>= print . doThing
