module Main where

import Graphics.UI.WXCore

main :: IO ()
main
  = run gui

gui :: IO ()
gui
  = do frame <- frameCreateTopFrame "Hello World"
       windowShow frame
       windowRaise frame
       return ()