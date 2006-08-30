{--------------------------------------------------------------------------------
  Author: Daan Leijen
  Demo that shows how one can manipulate pixels directly in an image
  to do fast customized drawing.
--------------------------------------------------------------------------------}

module Main where

import Graphics.UI.WXCore
import Graphics.UI.WX

rgbSize = sz 256 256

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do im <- imageCreateSized rgbSize
       pb <- imageGetPixelBuffer im
       fillGreenBlue pb
       bm  <- bitmapCreateFromImage im (-1)
       f   <- frame [text := "Paint demo", fullRepaintOnResize := False]
       sw  <- scrolledWindow f [on paintRaw := onpaint bm
                               ,virtualSize := rgbSize, scrollRate := sz 10 10
                               ,fullRepaintOnResize := False ]
       set f [layout     := fill $ widget sw
             ,clientSize := sz 200 200
             ,on closing := do bitmapDelete bm
                               imageDelete im
                               propagateEvent
             ]
       return ()
  where
    onpaint :: Bitmap () -> DC b -> Rect -> [Rect] -> IO ()
    onpaint bm dc viewArea dirtyAreas
      = do drawBitmap dc bm pointZero True []

    fillGreenBlue pb
      = mapM_ (drawGreenBlue pb) [Point x y  | x <- [0..255], y <- [0..255]]

    drawGreenBlue pb point
      = pixelBufferSetPixel pb point (colorRGB 0 (pointX point) (pointY point))


           

           
