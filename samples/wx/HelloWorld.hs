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
       quit   <- menuQuit file "&Quit\tCtrl+Q" "Quit the demo" []

       -- create Help menu
       help   <- menuHelp "&Help" []
       about  <- menuAbout help "&About" "About wxHaskell" []

       -- create statusbar field
       status <- statusField [text := "Welcome to wxHaskell"]

       -- set the statusbar and menubar, and add menu item event handlers
       set f [statusbar := [status]
             ,menubar   := [file,help]
             ,on (menu about) := infoDialog f "About wxHaskell" "This is a wxHaskell demo"
             ,on (menu quit)  := close f
             ]