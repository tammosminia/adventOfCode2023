module Grid where

import Util
import Data.List
import qualified Data.Map as Map

type Grid a = [[a]] -- rows, columns. 0,0 is top left
type Point = (Int, Int) -- (x, y)
data Direction = North | East | South | West

init :: [String] -> Grid Int
init = map initLine
  where
    initLine = map charToInt

get :: Grid a -> Point -> a
get g (x, y) = (g !! y) !! x

set :: Grid a -> Point -> a -> Grid a
set g (x, y) e = replace g y $ replace (g !! y) x e

mapGrid :: (a -> a) -> Grid a -> Grid a
mapGrid f = map (map f)

bottomRight :: Grid a -> Point
bottomRight m = (length (head m) - 1, length m - 1)

--not diagonal
neighbours :: Point -> Grid a -> [Point]
neighbours (x, y) m = left ++ right ++ top ++ bottom
  where
    (maxX, maxY) = bottomRight m
    left = if x > 0 then [(x - 1, y)] else []
    right = if x < maxX then [(x + 1, y)] else []
    top = if y > 0 then [(x, y - 1)] else []
    bottom = if y < maxY then [(x, y + 1)] else []

plus :: Point -> Point -> Point
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

allPoints :: Grid a -> [Point]
allPoints grid = [(x, y) | x <- [0..maxX], y <- [0..maxY]]
  where
    (maxX, maxY) = bottomRight grid

allDirections :: [Direction]
allDirections = [North, East, South, West]

pointsInDirection :: Grid a -> Point -> Direction -> [Point]
pointsInDirection grid (x, y) North = [(x, ny)| ny <- reverse [0..(y-1)]]
pointsInDirection grid (x, y) South = [(x, ny)| ny <- [(y+1)..(snd (bottomRight grid))]]
pointsInDirection grid (x, y) West = [(nx, y)| nx <- reverse [0..(x-1)]]
pointsInDirection grid (x, y) East = [(nx, y)| nx <- [(x+1)..(fst (bottomRight grid))]]

pointDiff :: Direction -> Point
pointDiff North = (0, -1)
pointDiff South = (0, 1)
pointDiff West = (-1, 0)
pointDiff East = (1, 0)

move :: Direction -> Point -> Point
move d p = plus p $ pointDiff d

