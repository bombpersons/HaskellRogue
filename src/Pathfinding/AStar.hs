{-# LANGUAGE RankNTypes #-}
module Pathfinding.AStar (
  Pos, Path, Grid, Cost, Heuristic, Neighbours,
  findPath
) where

import           Data.Array
import qualified Data.Map        as Map
import           Data.Maybe
import qualified Data.PQueue.Min as MinQueue

-- Debug
import           Debug.Trace

type Pos a = (a, a)
type Path a = [Pos a]
type Grid a e = Array (a, a) e

-- Cost function
type Cost a e n = (Pos a -> Pos a -> Grid a e -> n)

-- Heuristic function
type Heuristic a n = (Pos a -> Pos a -> n)

-- Generate a list of neighbours
type Neighbours a e = (Pos a -> Grid a e -> [Pos a])

-- Internal stuff.

-- Node for the openset.
-- Stores the 'f' score (the cost of the path up to this point)
-- Stores the actual position of this node on the grid.
data Node a n = Node n n (Pos a) deriving (Eq)
instance (Eq a, Num n, Ord n) => Ord (Node a n) where
  compare (Node f g _) (Node f' g' _) = compare (f+g) (f'+g')
type OpenSet a n = MinQueue.MinQueue (Node a n)

-- Stores the next position you'd need to go to if you were
-- walking back to the starting point.
type ClosedSet a n = Map.Map (Pos a) (Pos a, n)

findPath :: (Ord a, Ord e, Ord n, Eq a, Eq e, Num n) => Pos a -> Pos a -> Grid a e -> Cost a e n -> Heuristic a n -> Neighbours a e -> Maybe (Path a)
findPath start end grid cost heuristic neighbours = findPath' (MinQueue.singleton (Node 0 0 start)) (Map.singleton start (start, 0))
  where
    findPath' open closed = do
        minNode@(Node f g pos) <- MinQueue.getMin open
        let neighbourNodes = filterNeighbours pos f closed
            closed' = foldr (\p c -> Map.insert p (pos, f + cost pos p grid) c) closed neighbourNodes
            open' = foldr (\p c -> MinQueue.insert (Node (f + cost pos p grid) (heuristic pos p) p) c) (MinQueue.deleteMin open) neighbourNodes
        if pos == end then
          return $ flattenPath closed' [end]
        else
          findPath' open' closed' -- Keep going till we find a solution.

    filterNeighbours pos f closed = filter filterFunc $ neighbours pos grid
      where
        filterFunc nPos = case Map.lookup nPos closed of
                            Nothing -> True
                            Just (cameFrom, score) -> f + cost pos nPos grid < score

    flattenPath closed path@(p:ps) = case Map.lookup p closed of
                                      Nothing -> []
                                      Just (cameFrom, _) -> if cameFrom == start then
                                                              cameFrom : path
                                                            else
                                                              flattenPath closed (cameFrom : path)
