module Main where
import Graphics.UI.WX

bugtext = unlines [ "Bug: these button should react when clicked"
                  , "but the boxed one does not" ]

main = start $ do
         f <- frame [ text := "program" ]
         b1 <- button f [ text := "click me" ]
         b2 <- button f [ text := "click me" ]
         set b1 [ on command := set b1 [ text := "thanks!" ] ]
         set b2 [ on command := set b2 [ text := "thanks!" ] ]
         set f [ visible := True
               , layout := margin 10 $ column 4
                  [ label bugtext, widget b1, (boxed "" $ widget b2) ]
               ]
