module Main where

import Graphics.UI.WX

-- To reproduce the bug, copy the text from the lower text widget to
-- the upper text widget.

main = start $
       do f  <- frame [ text := "Problems with font" ]
          p  <- panel f []
          tcOne <- textCtrl p [ ]
          tcTwo <- textCtrl p [ ]
          set tcOne [ font := fontFixed ]
          set tcTwo [ font := fontFixed ]
          set tcTwo [ text := monoText ]
          set f [ layout := container p $ column 10 [ fill $ widget tcOne, fill $ widget tcTwo ]
                , size := Size 640 480
                ]

monoText :: String
monoText = "123456789\n" ++
           "      789"

{-
-- A nasty workaround for removing bug, it to periodically get and re-set the
-- text control contents:

          timer f [ interval   := 500
                  , on command :=  do xs <- get tcTwo text
                                      set tcTwo [ text := xs ]  
                  ]
-}
