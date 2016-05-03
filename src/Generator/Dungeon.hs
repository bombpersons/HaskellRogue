{-# LANGUAGE ScopedTypeVariables #-}
module Generator.Dungeon where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.ST

import           Data.Array
import           Data.Array.ST

import           Pathfinding.AStar

-- Pathfinding
cost :: Cost Int Char Int
cost (x, y) (x', y') grid = 1

heuristic :: Heuristic Int Int
heuristic (x, y) (x', y') = (abs (x - x')) + (abs (y - y')) -- Manhattan distance.

neighbours :: Neighbours Int Char
neighbours (x, y) grid = [ (x', y') | x' <- [x-1..x+1],
                                      y' <- [y-1..y+1],
                                      x' >= lx && x' <= hx,
                                      y' >= ly && y' <= hy,
                                      x-x' == 0 || y-y' == 0, -- only one dir
                                      x /= x' || y /= y', -- don't include ourselves.
                                      grid ! (x', y') /= '#' ]
  where ((lx, ly), (hx, hy)) = bounds grid

-- Settings.
gRoomSizeRange = (3, 20)
gMinSplit :: (Int, Int) -> (Int, Int)
gMinSplit (mn,mx) = ((floor (mnf + (mxf-mnf) * 0.3)), (floor (mxf - (mxf-mnf) * 0.3)))
  where
    mnf = fromIntegral mn
    mxf = fromIntegral mx

-- Data
data Room = Room Int Int Int Int deriving (Show)
data Corridor = Corridor [(Int, Int)] deriving (Show)
data RoomDungeon = RoomDungeon [Room] [Corridor]

data Dungeon = Dungeon (Array (Int, Int) Char) deriving (Show)

data Rect = Rect Int Int Int Int deriving (Show)

-- Functions
genDungeon :: (RandomGen g) => Int -> Int -> Int -> Rand g Dungeon
genDungeon w h depth = do
  rooms <- genRooms (Rect 1 1 w h) depth

  -- Flatten the rooms and corridors we generated into
  -- a 2d array of chars.
  let dat = runSTArray $ do
              -- Create an empty array
              a <- newArray ((1, 1), (w, h)) ' '

              -- Draw our rooms.
              drawRooms a rooms

              return a

  return $ Dungeon dat

drawRooms :: STArray s (Int, Int) Char -> [Room] -> ST s ()
drawRooms a rooms = sequence_ $ drawRoom a <$> rooms

drawRoom :: STArray s (Int, Int) Char -> Room -> ST s ()
drawRoom a (Room rx ry rw rh) = do
  drawFill a (Rect rx ry rw rh) '.'
  drawBox a (Rect rx ry rw rh) '#'

drawLineH :: STArray s (Int, Int) Char -> (Int, Int) -> Int -> Char -> ST s ()
drawLineH a (x, y) l c = sequence_ $ (\i -> writeArray a (i, y) c) <$> [x..(x+l-1)]

drawLineV :: STArray s (Int, Int) Char -> (Int, Int) -> Int -> Char -> ST s ()
drawLineV a (x, y) l c = sequence_ $ (\i -> writeArray a (x, i) c) <$> [y..(y+l-1)]

drawFill :: STArray s (Int, Int) Char -> Rect -> Char -> ST s ()
drawFill a (Rect x y w h) c = sequence_ $ (\i -> drawLineH a (x, i) w c) <$> [y..(y+h-1)]

drawBox :: STArray s (Int, Int) Char -> Rect -> Char -> ST s ()
drawBox a (Rect x y w h) c = do
  drawLineH a (x, y) w c
  drawLineH a (x, y + h - 1) w c
  drawLineV a (x, y) h c
  drawLineV a (x + w - 1, y) h c

splitRect :: Rect -> Int -> Bool -> (Rect, Rect)
splitRect (Rect x y w h) splitPoint True = (Rect x y splitPoint h, Rect (x + splitPoint) y (w - splitPoint) h)
splitRect (Rect x y w h) splitPoint False = (Rect x y w splitPoint, Rect x (y + splitPoint) w (h - splitPoint))

genRooms :: (RandomGen g) => Rect -> Int -> Rand g [Room]
genRooms r num = doSplit r num True
  where
    doSplit r@(Rect x y w h) depth axis
      | depth <= 0 = do
        room <- genRoom r
        return [room]
      | otherwise = do
          splitPoint <- getRandomR $ if axis then gMinSplit (0, w) else gMinSplit (0, h)
          let (leftRect, rightRect) = splitRect r splitPoint axis

          leftRooms <- doSplit leftRect (depth-1) (not axis)
          rightRooms <- doSplit rightRect (depth-1) (not axis)

          return $ leftRooms ++ rightRooms

-- Generate a room in an area
genRoom :: (RandomGen g) => Rect -> Rand g Room
genRoom (Rect x y w h) = do
  --return $ Room x y w h

  rX :: Int <- getRandomR (0, w - fst gRoomSizeRange)
  rY :: Int <- getRandomR (0, h - fst gRoomSizeRange)

  rW :: Int <- getRandomR gRoomSizeRange
  rH :: Int <- getRandomR gRoomSizeRange

  let xRemaining = w - rX
  let yRemaining = h - rY

  return $ Room (x+rX) (y+rY) (min xRemaining rW) (min yRemaining rH)
