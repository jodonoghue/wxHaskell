import Graphics.UI.WX

bugtext = unlines
 [ "Bug: the text in all these entry fields should be coloured,"
 , "coloured according to the text." ]


main = start $
  do f <- frame []
     e1 <- entry f [textColor := red, text := "should be red"]
     t1 <- textCtrlRich f [textColor := red, text := "should be red"]
     t2 <- textCtrlRich f [textColor := red]
     set t2 [text := "should be red"]
     t3 <- textCtrlRich f []
     set t3 [textColor := red, text := "should be red"]
     t4 <- textCtrlRich f [text := "should be black!", textColor := red]
     set f [ layout := column 4 [ label bugtext
                                , widget e1
                                , widget t1
                                , widget t2
                                , widget t3
                                , widget t4 ]]
