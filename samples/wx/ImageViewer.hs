{-----------------------------------------------------------------------------------------
 Copyright (c) Daan Leijen 2003
 wxWindows License.

 An image viewer in wxHaskell.
-----------------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WXH (bitmapDelete, bitmapCreateFromFile, bitmapGetSize, dcClear)
import Graphics.UI.WX

main :: IO ()
main
  = start imageViewer

-- Specify image files.
imageFiles
   = [("Image files",["*.bmp","*.jpg","*.gif","*.png"])
     ,("Portable Network Graphics (*.png)",["*.png"])
     ,("BMP files (*.bmp)",["*.bmp"])
     ,("JPG files (*.jpg)",["*.jpg"])
     ,("GIF files (*.gif)",["*.gif"])
     ]

-- The image viewer.
imageViewer :: IO ()
imageViewer
  = do -- the main frame
       f      <- frame [text := "ImageViewer"]

       -- use a mutable variable to hold the image
       vbitmap <- varCreate Nothing

       -- add a scrollable window widget in the frame
       sw     <- scrolledWindow f [scrollRate := sz 10 10, on paint := onPaint vbitmap]

       -- create file menu
       file   <- menuPane      [text := "&File"]
       mclose <- menuItem file [text := "&Close\tCtrl+C", help := "Close the image", enable := False]
       open   <- menuItem file [text := "&Open\tCtrl+O",  help := "Open an image"]
       menuLine file
       quit   <- menuQuit file [text := "&Quit\tCtrl+Q", help := "Quit the demo"]

       -- create Help menu
       hlp    <- menuHelp      [text := "&Help"]
       about  <- menuAbout hlp [text := "&About...", help := "About ImageViewer"]

       -- create statusbar field
       status <- statusField   [text := "Welcome to the wxHaskell ImageViewer"]

       -- set the statusbar, menubar, layout, and add menu item event handlers
       set f [statusbar        := [status]
             ,menubar          := [file,hlp]
             ,layout           := fill (widget sw)
             ,clientSize       := sz 300 200
             ,on (menu about)  := infoDialog f "About ImageViewer" "This is a wxHaskell demo"
             ,on (menu quit)   := close f
             ,on (menu open)   := onOpen f sw vbitmap mclose status 
             ,on (menu mclose) := onClose  sw vbitmap mclose status
             ,on closing       :~ \previous -> do{ closeImage vbitmap; previous }
             ]

  where
    onOpen :: Frame a -> ScrolledWindow b -> Var (Maybe (Bitmap ())) -> MenuItem c -> StatusField -> IO ()
    onOpen f sw vbitmap mclose status
      = do mbfname <- fileOpenDialog f True True "Open image" imageFiles "" ""
           case mbfname of
             Nothing    -> return ()
             Just fname -> openImage sw vbitmap mclose status fname

    onClose sw vbitmap mclose status
      = do closeImage vbitmap
           set mclose [enable := False]
           set sw     [virtualSize := sz 0 0]
           set status [text := ""]
           repaint sw

    closeImage vbitmap
      = do mbBitmap <- varSwap vbitmap Nothing
           case mbBitmap of
             Nothing -> return ()
             Just bm -> bitmapDelete bm

    openImage sw vbitmap mclose status fname
      = do -- load the new bitmap
           bm <- bitmapCreateFromFile fname  -- can fail with exception
           closeImage vbitmap
           varSet vbitmap (Just bm)
           set mclose [enable := True]
           set status [text := fname]
           -- reset the scrollbars 
           bmsize <- bitmapGetSize bm
           set sw [virtualSize := bmsize]
           repaint sw
       `catch` \err -> repaint sw

    onPaint vbitmap dc viewRect updateAreas
      = do mbBitmap <- varGet vbitmap
           case mbBitmap of
             Nothing -> dcClear dc
             Just bm -> drawBitmap dc bm pointZero False []
