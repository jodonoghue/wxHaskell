module Main where

import Graphics.UI.WXCore

main :: IO ()
main
  = run gui

gui :: IO ()
gui
  = do frame <- frameCreate objectNull idAny "Hello world" rectZero frameDefaultStyle
       windowShow frame
       windowRaise frame
       return ()