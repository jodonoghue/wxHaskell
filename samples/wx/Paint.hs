module Main where

import Graphics.UI.WXCore
import Graphics.UI.WX

rgbSize = sz 255 255

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do pb  <- pixelBufferCreate rgbSize
       fill pb
       im  <- imageCreateFromPixelBuffer pb
       bm  <- bitmapCreateFromImage im (-1)
       f   <- frame [text := "Paint demo"]
       p   <- panel f [on paintRaw := onpaint bm, clientSize := rgbSize ]
       set f [layout := fill $ widget p]
       return ()
  where
    onpaint bm dc viewArea dirtyAreas
      = do drawBitmap dc bm 

    fill pb
      = mapM_ (drawPixel pb) [Point x y  | x <- [0..255], y <- [0..255]]

    drawPixel pb point
      = pixelBufferSetPixel pb point (colorRGB 0 (pointX point) (pointY point))


           

           
