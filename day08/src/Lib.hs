module Lib
  ( solve
  , solve2
  ) where

import Data.Foldable (minimumBy, for_)
import Data.List.Split

type Size = (Int, Int)

toLayers :: Size -> [a] -> [[a]]
toLayers (w, h) = chunksOf (w * h)

toRows :: Size -> [a] -> [[a]]
toRows (w, _) = chunksOf w

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

solve :: IO ()
solve = do
  content <- readFile "input.txt"
  let layers = toLayers (25, 6) content
  let bestLayer = minimumBy (\a b -> compare (count '0' a) (count '0' b)) layers
  let ones = count '1' bestLayer
  let twos = count '2' bestLayer
  print $ ones * twos

combinePixel :: Char -> Char -> Char
combinePixel '0' _ = '0'
combinePixel '1' _ = '1'
combinePixel _ x = x

combineTwoLayers :: String -> String -> String
combineTwoLayers = zipWith combinePixel

combineLayers :: [String] -> String
combineLayers (x:xs) = foldl combineTwoLayers x xs

solve2 :: IO ()
solve2 = do
  content <- readFile "input.txt"
  let size = (25, 6)
  let layers = toLayers size content
  let combined = combineLayers layers
  let rows = toRows size combined
  let printable =  map (map (\x -> if x == '0' then ' ' else '*')) rows
  for_ printable putStrLn