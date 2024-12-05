{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, lookup, assocs, insertWith)
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
          ordMap = until (\x -> iterateMap x == x) iterateMap initMap
          iterateMap :: Map Word (Set Word) -> Map Word (Set Word)
          iterateMap m = foldr (\(k :: Word, vs :: Set Word) m1 -> foldr (\v m2 -> foldr (Data.Map.insertWith Data.Set.union k) m2 $ Data.Map.lookup v m2) m1 vs) m $ Data.Map.assocs m
          initMap :: Map Word (Set Word)
          initMap = Data.Map.fromList $ fmap (Data.Bifunctor.second Data.Set.singleton) constraints

doThing :: String -> String
doThing input = let
    inputLines = lines input
    orderConstraints = (\l -> (readWord (takeWhile isDigit l), readWord (drop 1 $ dropWhile (/= '|') l))) <$> takeWhile (not . null) inputLines
    printOrders = fmap readWord . splitOn "," <$> filter (not . null) (dropWhile (not . null) inputLines)
    order = buildPrintOrder orderConstraints
--    isOrdered :: [Word] -> Bool
--    isOrdered pages = sortBy order pages == pages
    isOrdered :: [Word] -> Bool
    isOrdered (x:ys) = isOrdered ys && all (\y -> case order x y of
        GT -> False
        _ -> True) ys
    isOrdered [_] = True
    isOrdered _ = True
    in show . sum . traceShowId  . fmap middle $ filter isOrdered printOrders
    where readWord :: String -> Word
          readWord = read
          middle :: [Word] -> Word
          middle l@(_:_:_:_) = middle $ tail $ init l
          middle [l, r]      = l + r
          middle [l]         = l
          middle []          = 0




main :: IO ()
main = getContents >>= print . doThing
