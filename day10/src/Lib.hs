module Lib
  ( solve
  , solve2
  ) where

import Control.Monad (guard)
import Data.List (groupBy, maximumBy, partition, sortBy, transpose)
import qualified Data.Set as S

positions :: [String] -> S.Set (Integer, Integer)
positions xs =
  S.fromList $ do
    (y, row) <- zip [0 ..] xs
    (x, cell) <- zip [0 ..] row
    guard $ cell == '#'
    pure (x, y)

roundTo n f = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)

wrapTo2Pi x =
  if x < 0
    then 2 * pi + x
    else x

angle :: (Integer, Integer) -> (Integer, Integer) -> Double
angle (x1, y1) (x2, y2) = roundTo 5 . wrapTo2Pi $ atan2 dx (-dy)
  where
    dx = fromInteger $ x2 - x1
    dy = fromInteger $ y2 - y1

solve :: IO ()
solve = do
  content <- readFile "input.txt"
  let linesOfFiles = lines content
  let pos = positions linesOfFiles
  let all = S.map (\x -> (S.size $ S.map (angle x) pos, x)) pos
  let (count, position) = maximumBy (\x y -> compare (fst x) (fst y)) all
  print position -- (19,11)

groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = go []
  where
    go acc comp [] = acc
    go acc comp (h:t) =
      let (hs, nohs) = partition (comp h) t
       in go ((h : hs) : acc) comp nohs

positions2 :: (Integer, Integer) -> [String] -> [[(Integer, Integer)]]
positions2 p xs =
  groupBy2 (\x y -> angle p x == angle p y) $ do
    (y, row) <- zip [0 ..] xs
    (x, cell) <- zip [0 ..] row
    guard $ cell == '#'
    pure (x, y)

manhattan :: (Integer, Integer) -> (Integer, Integer) -> Integer
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

solve2 :: IO ()
solve2 = do
  content <- readFile "input.txt"
  let turret = (19, 11)
  let linesOfFiles = lines content
  let pos = positions2 turret linesOfFiles
  let x = sortBy (\x y -> compare (angle turret $ head x) (angle turret $ head y)) pos
  let y = fmap (sortBy (\a b -> compare (manhattan turret a) (manhattan turret b))) x
  let z = concat $ transpose y
  let (x, y) = z !! 199
  print $ x * 100 + y