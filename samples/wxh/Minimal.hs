module Main where

import Graphics.UI.WXH

main :: IO ()
main
  = run gui

gui :: IO ()
gui
  = do frame <- frameCreateTopFrame "Hello World"
       windowShow frame
       windowRaise frame
       return ()