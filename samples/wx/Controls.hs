{--------------------------------------------------------------------------------
   Controls demo.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore 

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do -- main gui elements: frame, panel, text control, and the notebook
       f       <- frame [text := "Controls"]
       p       <- panel f []
       nb      <- notebook p []
       textlog <- textCtrl p WrapLine [enable := False]

       -- use text control as logger
       textCtrlMakeLogActiveTarget textlog
       logMessage "logging enabled"              
       -- set f [on closing :~ \prev -> do logSetActiveTarget oldlog; logDelete log; prev]

       -- button page
       p1   <- panel  nb [text := "buttons"]
       ok   <- button p1 [text := "Ok", on command := logMessage "ok button pressed"]
       quit <- button p1 [text := "Quit", on command := close f]

       -- radio box page
       p2   <- panel  nb [text := "radio box"]
       let rlabels = ["first", "second", "third"]
       r1   <- radioBox p2 Vertical rlabels   [text := "radio box", on command ::= logSelect rlabels]
       r2   <- radioBox p2 Horizontal rlabels [tooltip := "radio group two", on command ::= logSelect rlabels ]
       rb1  <- button   p2 [text := "disable", on command ::= onEnable r1]

       -- choice
       p3   <- panel nb [text := "choice box"]
       let clabels = ["noot","mies","aap"]
       c1   <- choice p3 False clabels [tooltip := "unsorted choices", on command ::= logSelect clabels]
       c2   <- choice p3 True  clabels [tooltip := "sorted choices", on command ::= logSelect clabels]
       cb1  <- button p3 [text := "disable", on command ::= onEnable c1]

       -- list box page
       p4   <- panel nb [text := "list box"]
       sl1  <- singleListBox p4 False clabels 
                  [tooltip := "unsorted single-selection listbox", on command ::= logSelect clabels]
       sl2  <- singleListBox p4 True clabels 
                  [tooltip := "sorted listbox", on command ::= logSelect clabels]
       sc1  <- checkBox p4 [text := "enable the listbox", checked := True, on command := set sl1 [enable :~ not]]

       -- slider/gauge page
       p5   <- panel nb [text := "slider"]
       s    <- hslider p5 True {- show labels -} 1 100 [selection := 50]
       g    <- hgauge  p5 100 [selection := 50]
       set s [on command := do{ i <- get s selection; set g [selection := i]} ]

       -- specify layout
       set f [layout :=
                container p $
                column 0
                 [ tabs nb
                    [container p1 $ margin 10 $ floatCentre $ row 5 [widget ok, widget quit]
                    ,container p2 $ margin 10 $ column 5 [ hstretch $ widget rb1
                                                         , row 0 [floatLeft $ widget r1
                                                                 ,floatRight $ widget r2]]
                    ,container p3 $ margin 10 $ column 5 [ hstretch $ widget cb1
                                                         , row 0 [floatLeft $ widget c1
                                                                 ,floatRight $ row 5 [label "sorted: ", widget c2]]]
                    ,container p4 $ margin 10 $ column 5 [ hstretch  $ widget sc1
                                                         , floatLeft $
                                                           row 0 [widget sl1, widget sl2]]
                    ,container p5 $ margin 10 $ column 5 [ hfill $ widget s
                                                         , hfill $ widget g
                                                         , glue
                                                         ]
                    ]
                 , hfill $ widget textlog
                 ]
             , clientSize := sz 400 300 ]
       return ()

  where
    -- logSelect :: (Selection w, Items w String) => [String] -> w -> IO ()
    logSelect labels w
      = do i <- get w selection
           s <- get w (item i)
           logMessage ("selected index: " ++ show i ++ ": " ++ s)
           

    onEnable w b
      = do set w [enable :~ not]
           enabled <- get w enable
           set b [text := (if enabled then "disable" else "enable")]