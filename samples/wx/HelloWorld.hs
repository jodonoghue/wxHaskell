{- demonstrates the use of a simple menu, statusbar, and dialog -}
module Main where

import Graphics.UI.WX

main :: IO ()
main
  = start hello

hello :: IO ()
hello
  = do f      <- frame [text := "Hello world!", clientSize := sz 300 200]

       -- create file menu
       file   <- menuList  "&File" []
       about  <- menuAbout file "&About.." "About wxHaskell" []
       menuLine file
       exit   <- menuExit file "&Quit\tCtrl+Q" "Quit the demo" []

       -- create statusbar field
       status <- statusField 1 [text := "Welcome to wxHaskell"]

       -- set the statusbar and menubar, and add menu item event handlers
       set f [statusbar := [status]
             ,menubar   := [file]
             ,on (menu about) := infoDialog f "About wxHaskell" "This is a wxHaskell demo"
             ,on (menu exit)  := close f
             ]