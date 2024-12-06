{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import Data.Map (Map)
import qualified Data.Map (empty, insert, filter, keys, lookup, insertWith)
import Data.Set (Set)
import qualified Data.Set (empty, singleton, union, null)
import Data.Maybe (listToMaybe, isJust, fromJust)

import Debug.Trace (trace)

data Direction = LookUp | LookDown | LookLeft | LookRight deriving (Show, Eq, Ord)

turnRight :: Direction -> Direction
turnRight LookUp    = LookRight
turnRight LookRight = LookDown
turnRight LookDown  = LookLeft
turnRight LookLeft  = LookUp

data Field = Visited (Set Direction) | Blocked deriving (Show, Eq)

instance Semigroup Field where
    Visited l <> Visited r = Visited (l `Data.Set.union` r)
    _ <> _ = Blocked

type GameMap = Map (Word, Word) Field

parseGameMap :: String -> GameMap
parseGameMap = foldl (\m (line, lineNum) -> foldl (\m1 (col, columnNum) -> Data.Map.insert (lineNum, columnNum) (parsePosition col) m1) m $ zip line [1 :: Word ..]
    ) Data.Map.empty . flip zip [1 :: Word ..] . lines
    where parsePosition :: Char -> Field
          parsePosition '#' = Blocked
          parsePosition '^' = Visited $ Data.Set.singleton LookUp
          parsePosition _ = Visited Data.Set.empty

findFirstPosition :: GameMap -> Maybe (Word, Word)
findFirstPosition m = let filtered = Data.Map.filter (\case Visited s -> not . Data.Set.null $ s; Blocked -> False) m in listToMaybe $ Data.Map.keys filtered

data GameStep = GameStep GameMap Direction (Word, Word)

printStep :: GameStep -> String
printStep (GameStep m d pos) =
    (unlines . takeWhile (not . null) $ map printLine [1 :: Word ..]) ++ "\n" ++ show (d, pos)
    where printLine :: Word -> String
          printLine l = map (printField . fromJust) . takeWhile isJust $ map (\c -> Data.Map.lookup (l, c) m) [1 :: Word ..]
          printField Blocked = '#'
          printField (Visited s) = if Data.Set.null s then '.' else 'X'

stepGame :: GameStep -> GameStep
stepGame (GameStep m dir (x, y)) = let
    (x1, y1) = case dir of
        LookUp    -> (x - 1, y)
        LookDown  -> (x + 1, y)
        LookLeft  -> (x, y - 1)
        LookRight -> (x, y + 1)
    (pos2@(x2, y2), dir2) = case Data.Map.lookup (x1, y1) m of 
        -- Nothing        -> ((x, y), turnRight dir)
        -- The instructions say to leave the field, but for the fixpoint iteration this is equivalent
        Nothing        -> ((x, y), dir)
        (Just Blocked) -> ((x, y), turnRight dir)
        _              -> ((x1, y1), dir)
    in GameStep (Data.Map.insertWith (<>) (x2, y2) (Visited $ Data.Set.singleton dir2) m) dir2 pos2

doThing :: String -> String
doThing input = let
    gameMap = parseGameMap input
    position = findFirstPosition gameMap
    in case position of
    Nothing -> "Couldn't get starting position :("
    Just pos -> let
        in show . countVisited . (\(GameStep m _ _) -> m) $ fixPoint stepGame (\s@(GameStep m1 _ _) (GameStep m2 _ _) -> m1 == {-trace (printStep s)-} m2) (GameStep gameMap LookUp pos)

    where fixPoint :: (a -> a) -> (a -> a -> Bool) -> a -> a
          fixPoint f eq = until (\y -> eq (f y) y) f
          countVisited :: GameMap -> Int
          countVisited = length . Data.Map.keys . Data.Map.filter (\case
            Blocked -> False
            Visited s -> not $ Data.Set.null s)

main :: IO ()
main = getContents >>= print . doThing
