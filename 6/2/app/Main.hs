{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Bits (Bits (shiftL), (.|.))
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as Data.IntMap (empty, filter, insert, insertWith, keys, lookup)
import Data.Maybe (fromJust, isJust, listToMaybe)
import GHC.Exts (Int#, orI#)
import Control.Parallel.Strategies (parMap, rparWith, rdeepseq)
import Debug.Trace (traceShowId)

data Direction = LookUp | LookDown | LookLeft | LookRight deriving (Show, Eq, Ord)

turnRight :: Direction -> Direction
turnRight LookUp = LookRight
turnRight LookRight = LookDown
turnRight LookDown = LookLeft
turnRight LookLeft = LookUp

data Field = Unvisited | Visited !Int# | Blocked deriving (Show, Eq)

fieldFromDir :: Direction -> Field
fieldFromDir LookUp    = Visited 0x01#
fieldFromDir LookRight = Visited 0x02#
fieldFromDir LookDown  = Visited 0x04#
fieldFromDir LookLeft  = Visited 0x08#

instance Semigroup Field where
  Visited v1 <> Visited v2 = Visited (orI# v1 v2)
  Unvisited  <> y          = y
  x          <> Unvisited  = x
  _          <> _          = Blocked

type GameIntMap = IntMap Field

makeIndex :: Int -> Int -> Int
makeIndex l c = l .|. (c `shiftL` 32)

parseGameIntMap :: String -> GameIntMap
parseGameIntMap =
  foldl
    ( \m (line, lineNum) ->
        foldl (\m1 (col, columnNum) -> Data.IntMap.insert (makeIndex lineNum columnNum) (parsePosition col) m1)
            m $ zip line [1 :: Int ..]
    )
    Data.IntMap.empty
    . flip zip [1 :: Int ..]
    . lines
  where
    parsePosition :: Char -> Field
    parsePosition '#' = Blocked
    parsePosition '^' = fieldFromDir LookUp
    parsePosition _ = Unvisited

findFirstPosition :: GameIntMap -> Maybe Int
findFirstPosition m = let filtered = Data.IntMap.filter (\case Visited {} -> True; _ -> False) m in listToMaybe $ Data.IntMap.keys filtered

data GameStep = GameStep !GameIntMap !Direction !Int

printStep :: GameStep -> String
printStep (GameStep m d pos) =
  (unlines . takeWhile (not . null) $ map printLine [1 :: Int ..]) ++ "\n" ++ show (d, pos)
  where
    printLine :: Int -> String
    printLine l = map (printField . fromJust) . takeWhile isJust $ map (\c -> Data.IntMap.lookup (makeIndex l c) m) [1 :: Int ..]
    printField Blocked = '#'
    printField Unvisited = '.'
    printField (Visited {}) = 'X'

nextStep :: Int -> Direction -> Int
nextStep pos dir = case dir of
  LookUp -> pos - 1
  LookDown -> pos + 1
  LookLeft -> pos - (1 `shiftL` 32)
  LookRight -> pos + (1 `shiftL` 32)

looped :: GameStep -> Bool
looped (GameStep m dir pos) = isJust $ Data.IntMap.lookup (nextStep pos dir) m

stepGame :: GameStep -> GameStep
stepGame (GameStep m dir pos) =
  let pos1 = nextStep pos dir
      (pos2, dir2) = case Data.IntMap.lookup pos1 m of
        -- The instructions say to leave the field, but for the fixpoint iteration this is equivalent
        Nothing -> (pos, dir)
        (Just Blocked) -> (pos, turnRight dir)
        _ -> (pos1, dir)
   in GameStep (Data.IntMap.insertWith (<>) pos2 (fieldFromDir dir2) m) dir2 pos2

startingPoints :: GameStep -> [GameStep]
startingPoints i@(GameStep im _ _) = loop i
    where
    loop :: GameStep -> [GameStep]
    loop s@(GameStep m d p) = let
        stepped@(GameStep m2 _ _) = stepGame s
        next = nextStep p d
        in if m2 == m then [] else case Data.IntMap.lookup (nextStep p d) m of
        Just Unvisited ->  GameStep (Data.IntMap.insert next Blocked im) d p : startingPoints stepped
        _ -> startingPoints stepped

doThing :: String -> String
doThing input =
  let gameIntMap = parseGameIntMap input
      position = findFirstPosition gameIntMap
   in case position of
        Nothing -> "Couldn't get starting position :("
        Just pos ->
          let
           in show . length . filter id . parMap rdeepseq (looped . runGame) $ startingPoints (GameStep gameIntMap LookUp pos)
  where
    fixPoint :: (a -> a) -> (a -> a -> Bool) -> a -> a
    fixPoint f eq = until (\y -> eq (f y) y) f
    runGame :: GameStep -> GameStep
    runGame = fixPoint stepGame (\(GameStep m1 _ _) (GameStep m2 _ _) -> m1 == m2)

main :: IO ()
main = getContents >>= print . doThing
