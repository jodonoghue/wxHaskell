 module Main where
 
 import Graphics.UI.WX
 
 main :: IO ()
 main
   = start
     $ do f <- frame [text := "Checkbox Test"]
          c <- checkBox f [text := "See the space -->"]
          t <- staticText f [text := "<-- between the arrows?"]
          set f [layout := margin 10 $ row 0 [widget c, widget t]]
