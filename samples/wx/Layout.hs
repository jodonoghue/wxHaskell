{-----------------------------------------------------------------------------------------
Layout demo.
-----------------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do f      <- frame  [text := "Layout test"]
       p      <- panel  f []                       -- panel for color and tab management.
       ok     <- button p [text := "Ok", on command := close f]
       can    <- button p [text := "Cancel"]
       xinput <- textEntry p AlignRight [text := "100"]
       yinput <- textEntry p AlignRight [text := "100"]

       set p [defaultButton := ok]
       set f [layout := container p $
                        margin 10 $
                        column 5 [boxed "coordinates" (grid 5 5 [[label "x:", hfill $ widget xinput]
                                                                ,[label "y:", hfill $ widget yinput]])
                                 ,floatBottomRight $ row 5 [widget ok,widget can]]
             ]
       return ()