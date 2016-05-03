module Main where

import           Control.Monad.Random
import           Data.Array

import           Generator.Dungeon
import qualified Graphics
import           Pathfinding.AStar

import           UI.NCurses

-- TODO Benchmark Pathfinding.
-- Make a test thing maybe?

drawDungeon :: Window -> Dungeon -> (Int, Int) -> Curses ()
drawDungeon wnd (Dungeon dat) pathTo = do
  let ((lw, lh), (w, h)) = bounds dat

  Graphics.array2d wnd 0 0 dat
  Graphics.rectangle wnd 0 0 w h

  -- Path from the top corner to the bottom corner.
  let coord1 = (2, 2)
      coord2 = pathTo
  let mPath = findPath coord1 coord2 dat cost heuristic neighbours
  case mPath of
    Just path -> do
      updateWindow wnd $ do
        sequence_ $ (\(x, y) -> do
          moveCursor (fromIntegral (y-lh)) (fromIntegral (x-lw))
          drawGlyph (Glyph '0' [])) <$> path
    Nothing -> return ()

  -- updateWindow wnd $ do
  --   moveCursor 0 0
  --   drawString $ show $ neighbours (w, h) dat

main :: IO ()
main = do
  runCurses $ do
    setEcho False
    w <- defaultWindow
    loop w

  where
    loop w = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop w
            Just ev' -> do
              case ev' of
                EventMouse _ (MouseState (x, y, _) _ _ _ _) -> do
                  drawDungeon w dungeon (fromIntegral x, fromIntegral y)
                  render
                  loop w
                EventCharacter c -> if c /= 'q' then loop w else return ()

    dungeon = evalRand (genDungeon 100 50 4) (mkStdGen 777)
