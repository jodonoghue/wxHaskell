{-----------------------------------------------------------------------------------------
 Copyright (c) Daan Leijen 2003
 wxWindows License.

 An image viewer in wxHaskell.
 Demonstrates: 
 - menus, toolbars and the statusbar
 - standard file dialogs
 - scrollable windows
 - drawing on DC's (device contexts)
 - image handling
-----------------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WXCore ( bitmapDelete, bitmapCreateFromFile, bitmapGetSize, dcClear)
import Graphics.UI.WX 

main :: IO ()
main
  = start imageViewer

-- Specify image files for the file open dialog.
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
  = do -- the main frame, we use 'fullRepaintOnResize' to prevent flicker on resize
       f      <- frame [text := "ImageViewer", image := "../bitmaps/eye.ico", fullRepaintOnResize := False]

       -- use a mutable variable to hold the image
       vbitmap <- varCreate Nothing

       -- add a scrollable window widget in the frame
       sw     <- scrolledWindow f [scrollRate := sz 10 10, on paint := onPaint vbitmap
                                  ,bgcolor := white, fullRepaintOnResize := False]

       -- create file menu
       file   <- menuPane      [text := "&File"]
       mclose <- menuItem file [text := "&Close\tCtrl+C", help := "Close the image", enabled := False]
       open   <- menuItem file [text := "&Open\tCtrl+O",  help := "Open an image"]
       menuLine file
       quit   <- menuQuit file [help := "Quit the demo"]

       -- create Help menu
       hlp    <- menuHelp      []
       about  <- menuAbout hlp [help := "About ImageViewer"]

       -- create Toolbar
       tbar   <- toolBar f []
       toolMenu tbar open  "Open"  "../bitmaps/fileopen16.png" []
       toolMenu tbar about "About" "../bitmaps/wxwin16.png"    []

       -- create statusbar field
       status <- statusField   [text := "Welcome to the wxHaskell ImageViewer"]

       -- set the statusbar, menubar, layout, and add menu item event handlers
       -- note: set the layout before the menubar!
       set f [layout           := column 1 [hfill $ hrule 1  -- add divider between toolbar and scrolledWindow
                                           ,fill (widget sw)]
             ,statusbar        := [status]
             ,menubar          := [file,hlp]
             ,on (menu about)  := infoDialog f "About ImageViewer" "This is a wxHaskell demo"
             ,on (menu quit)   := close f
             ,on (menu open)   := onOpen f sw vbitmap mclose status 
             ,on (menu mclose) := onClose  sw vbitmap mclose status
             ,on closing       :~ \previous -> do{ closeImage vbitmap; previous }
             ]

  where
    onOpen :: Frame a -> ScrolledWindow b -> Var (Maybe (Bitmap ())) -> MenuItem c -> StatusField -> IO ()
    onOpen f sw vbitmap mclose status
      = do mbfname <- fileOpenDialog f False {- change current directory -} True "Open image" imageFiles "" ""
           case mbfname of
             Nothing    -> return ()
             Just fname -> openImage sw vbitmap mclose status fname

    onClose sw vbitmap mclose status
      = do closeImage vbitmap
           set mclose [enabled := False]
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
           set mclose [enabled := True]
           set status [text := fname]
           -- reset the scrollbars 
           bmsize <- bitmapGetSize bm
           set sw [virtualSize := bmsize]
           repaint sw
       `catch` \err -> repaint sw

    onPaint vbitmap dc viewArea
      = do mbBitmap <- varGet vbitmap
           case mbBitmap of
             Nothing -> dcClear dc
             Just bm -> drawBitmap dc bm pointZero False []
