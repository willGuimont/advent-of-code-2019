module Lib
  ( solve
  ) where

import Control.Monad (guard, forM, forM_)
import qualified Data.Set as S

positions :: [String] -> S.Set (Integer, Integer)
positions xs =
  S.fromList $ do
    (y, row) <- zip [0 ..] xs
    (x, cell) <- zip [0 ..] row
    guard $ cell == '#'
    pure (x, y)

roundTo n f = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)

angle :: (Integer, Integer) -> (Integer, Integer) -> Double
angle (x1, y1) (x2, y2) = atan2 dy dx
  where
    dx = fromInteger $ x2 - x1
    dy = fromInteger $ y2 - y1

solve :: IO ()
solve = do
  content <- readFile "input.txt"
  let linesOfFiles = lines content
  let pos = positions linesOfFiles
  let all = S.map (\x -> S.size $ S.map (roundTo 5 . angle x) pos) pos 
  
  print $ maximum all