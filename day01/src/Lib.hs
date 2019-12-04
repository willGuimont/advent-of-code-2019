module Lib
  ( solve
  , solve2
  ) where

apply :: a -> (a -> b) -> b
apply x f = f x

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

solve :: IO ()
solve = do
  content <- readFile "input.txt"
  let linesOfFiles = lines content
  linesOfFiles |> map read |> map (\x -> x `quot` 3 - 2) |> sum |> print

computeFuelNeeded :: Integer -> Integer
computeFuelNeeded x
  | v > 0 = v + computeFuelNeeded v
  | otherwise = 0
  where v = x `quot` 3 - 2

solve2 :: IO ()
solve2 = do
  content <- readFile "input.txt"
  let linesOfFiles = lines content
  linesOfFiles |> map read |> map computeFuelNeeded |> sum |> print
