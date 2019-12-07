module Lib
  ( solve
  , solve2
  ) where

import Safe (headMay)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (join)
import Data.Maybe (isJust)
import Debug.Trace

apply :: a -> (a -> b) -> b
apply x f = f x

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = apply x f

newtype Orbit =
  Orbit (String, String)

parseOrbit :: Parser Orbit
parseOrbit = do
  center <- many1 alphaNum
  char ')'
  orbiter <- many1 alphaNum
  return $ Orbit (center, orbiter)

parseAll :: Parser [Orbit]
parseAll = sepBy parseOrbit (char '\n')

data Tree
  = Nil
  | Tre (String, [Tree], Tree)
  deriving (Show, Eq)

thatOrbit :: String -> [Orbit] -> [String]
thatOrbit orbitee orbits = orbits |> filter (\(Orbit (a, b)) -> a == orbitee) |> map (\(Orbit (a, b)) -> b)

countOrbit :: Tree -> [Orbit] -> Int
countOrbit Nil _ = 0
countOrbit (Tre (a, [], _)) _ = 1
countOrbit (Tre (a, b, _)) orbits = sum $ map (`countOrbit` orbits) b

height :: Tree -> Int
height (Tre (a, [], _)) = 0
height (Tre (a, b, _)) = 1 + maximum (map height b)

makeTree :: [Orbit] -> Tree
makeTree = makeTree' "COM" Nil

makeTree' :: String -> Tree -> [Orbit] -> Tree
makeTree' orbitee parent orbits = self
  where
    self = Tre (orbitee, trees, parent)
    orbiters = thatOrbit orbitee orbits
    trees = orbiters |> map (\x -> makeTree' x self orbits)

directOrbits :: Tree -> Int
directOrbits tree = go tree 0
  where
    go tree count =
      case tree of
        Tre (_, [], _) -> 0
        Tre (a, b, _) -> length b + sum (map directOrbits b)

indirectOrbits :: Tree -> Int
indirectOrbits tree@(Tre (a, b, _)) = directOrbits tree + sum (map indirectOrbits b)

solve :: IO ()
solve = do
  content <- readFile "input.txt"
  let (Right orbit) = parse parseAll "orbits" content
  let tree = makeTree orbit
  print $ indirectOrbits tree

doObjectOrbit :: String -> Tree -> Bool
doObjectOrbit o (Tre (a, b, _)) = or ((a == o) : map (doObjectOrbit o) b)

find :: String -> Tree -> Maybe Tree
find _ Nil = Nothing
find o tree@(Tre (a, b, p))
  | a == o = Just tree
  | otherwise = join $ headMay $ filter (/= Nothing) $ map (find o) b

findCommonOrbiter :: Tree -> String -> Tree
findCommonOrbiter current@(Tre (a, b, p)) search
  | isJust (find search current) = current
  | otherwise = findCommonOrbiter p search

dist :: Tree -> String -> Int -> Int
dist Nil _ _ = 0
dist top@(Tre (a, b, p)) s count
  | s == a = count
  | null b = 0
  | otherwise = maximum $ map (\x -> dist x s (count + 1)) b

solve2 :: IO ()
solve2 = do
  content <- readFile "input.txt"
  let (Right orbit) = parse parseAll "orbits" content
  let tree = makeTree orbit
  let (Just you) = find "YOU" tree
  let commonOrbiter@(Tre (a, b, p)) = findCommonOrbiter you "SAN"
  print a
  print ((dist commonOrbiter "SAN" 0 + dist commonOrbiter "YOU" 0) - 2)
  print $ indirectOrbits tree

-- 190

--                            YOU
--                           /
--          G - H       J - K - L
--         /           /
--  COM - B - C - D - E - F
--                 \
--                  I - SAN