module Graphics where

import           Data.Array
import           UI.NCurses

array :: Window -> Array Integer Char -> Curses ()
array wnd a = do
  updateWindow wnd $ do
    sequence_ $ drawGlyph <$> (flip Glyph [] <$> a)

array2d :: Window -> Int -> Int -> Array (Int, Int) Char -> Curses ()
array2d wnd x y a = do
  updateWindow wnd $ do
    arrayLine lh

  where
    ((lw, lh), (hw, hh)) = bounds a
    arrayLine i
      | i < lh || i > hh = return ()
      | otherwise = do
        let a' = ixmap ((lw, i), (hw, i)) id a
        moveCursor (toInteger (y + (i - lh))) (toInteger x)
        sequence_ $ drawGlyph <$> (flip Glyph [] <$> a')
        arrayLine (i+1)


rectangle :: Window -> Int -> Int -> Int -> Int -> Curses ()
rectangle wnd x y w h = do
  updateWindow wnd $ do
    moveCursor (toInteger y) (toInteger x)
    drawLineH (Just $ Glyph '#' []) (toInteger w)
    drawLineV (Just $ Glyph '#' []) (toInteger h)
    moveCursor (toInteger y) (toInteger ((x + w) - 1))
    drawLineV (Just $ Glyph '#' []) (toInteger h)
    moveCursor (toInteger ((y + h) - 1)) (toInteger x)
    drawLineH (Just $ Glyph '#' []) (toInteger w)
