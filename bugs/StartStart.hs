import Graphics.UI.WX

bugtext = "Bug: clicking this button\n"
 ++ "(which closes the window)\n"
 ++ "causes a crash"

main = start f >> start f
 where
   f = do w <- frame []
          b <- button w [ text := "Crash me" , on command := close w ]
          set w [ layout := fill $ column 2 [ label bugtext, widget b ] ]
          return ()
