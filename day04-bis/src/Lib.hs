module Lib
    ( solve
    ) where

import Data.List.Split

byConsecutivePairs :: [a] -> [(a, a)]
byConsecutivePairs xs = zip xs (tail xs)

isNonDecreasing :: Ord a => [a] -> Bool
isNonDecreasing xs = all (uncurry (<=)) pairs
  where
    pairs = byConsecutivePairs xs

hasGroupOfTwo :: Eq a => [a] -> Bool
hasGroupOfTwo xs = or consecutiveEquals
  where
    pairs = byConsecutivePairs xs
    consecutiveEquals = map (uncurry (==)) pairs

hasGroupOfMaxTwo :: Eq a => [a] -> Bool
hasGroupOfMaxTwo xs = any (\x -> length x == 1) splitted
  where
    pairs = byConsecutivePairs xs
    consecutiveEquals = map (uncurry (==)) pairs
    splitted = splitOn [False] consecutiveEquals

apply :: a -> (a -> b) -> b
apply x f = f x

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

solve :: IO ()
solve = [108457..562041]
  |> map show
  |> filter hasGroupOfMaxTwo
  |> filter isNonDecreasing
  |> length
  |> print
