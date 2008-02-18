import Graphics.UI.WX

bugtext = unlines
 [ "Bug: the text in all these entry fields should be red,"
 , "it is black in the first two" ]


main = start $
  do f <- frame []
     e1 <- entry f [text := "hello", textColor := red]
     t1 <- textCtrlRich f [text := "bad", textColor := red]
     t2 <- textCtrlRich f [textColor := red]
     set t2 [text := "good"]
     set f [ layout := column 4 [ label bugtext
                                , widget e1
                                , widget t1
                                , widget t2 ]]
