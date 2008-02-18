import Graphics.UI.WX

bugtext = unlines
 [ "Bug: the contents of this listCtrl ('Aa' and 'Bb') should appear"
 , "in the following label, but they do not:"]

main :: IO ()
main = start gui

gui
  = do f <- frame [text := "Test"]
       pa <- panel f []
       lc <- listCtrl pa [columns := [("Col1", AlignLeft, 100), ("Col2", AlignLeft, 100)],
                          items := [["Aa", "Bb"]]
                         ]
       i <- get lc items
       let l = unlines $ [ bugtext, "----------8<-----------" ]
                         ++ map unwords i
                         ++        [ "----------8<-----------" ]
       set f [layout := container pa $ column 5 [ fill $ widget lc , label l ] ]
