{-----------------------------------------------------------------------------------------
 Copyright (c) Daan Leijen 2003
 wxWindows License.

 An image viewer in wxHaskell.
-----------------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WXH
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
       file   <- menuList  "&File" []
       mclose <- menuItem file "&Close\tCtrl+C" "Close the image" [enable := False]
       open   <- menuItem file "&Open\tCtrl+O" "Open an image" []
       menuLine file
       quit   <- menuQuit file "&Quit\tCtrl+Q" "Quit the demo" []

       -- create Help menu
       help   <- menuHelp "&Help" []
       about  <- menuAbout help "&About" "About ImageViewer" []

       -- create statusbar field
       status <- statusField [text := "Welcome to the wxHaskell ImageViewer"]

       -- set the statusbar, menubar, layout, and add menu item event handlers
       set f [statusbar       := [status]
             ,menubar         := [file,help]
             ,layout          := fill (widget sw)
             ,on (menu about) := infoDialog f "About ImageViewer" "This is a wxHaskell demo"
             ,on (menu quit)  := close f
             ,on (menu open)  := onOpen f sw vbitmap mclose status 
             ,on (menu mclose):= onClose  sw vbitmap mclose status
             ,on closing      :~ \prev -> do closeImage vbitmap; prev 
             ,clientSize       := sz 300 200
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
