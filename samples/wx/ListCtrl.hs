{--------------------------------------------------------------------------------
   List control demo.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore 


entries 
  = [("BouncingBalls.hs"           ,"2402"  ,"Jul 19 16:50")
    ,("ByeDemo.hs"                 ,"1414"  ,"Jul 13 23:18")
    ,("Camels.hs"                  ,"7633"  ,"Aug 20 11:57")
    ,("Controls.hs"                ,"3862"  ,"Aug 20 11:57")
    ,("HelloWorld.hs"              ,"1028"  ,"Aug 15 10:09")
    ,("ImageViewer.hs"             ,"3756"  ,"Aug 20 11:57")
    ,("Layout.hs"                  ,"1075"  ,"Jul 13 23:18")
    ,("ListCtrl.hs"                 ,"750"  ,"Sep  8 16:22")
    ,("Minimal.hs"                  ,"147"  ,"Jul 13 23:18")
    ,("Paint.hs"                   ,"1024"  ,"Aug 20 11:57")
    ,("Process.hs"                 ,"2261"  ,"Aug 20 11:57")
    ,("TimeFlows.hs"               ,"4929"  ,"Aug 20 11:57")
    ,("TimeFlowsEx.hs"             ,"8648"  ,"Aug 20 11:57")
    ,("desert.bmp"                ,"61302"  ,"Jul 13 23:31")
    ]

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do -- main gui elements: frame, panel, text control, and the notebook
       f       <- frame [text := "List Sample"]
       -- panel: just for the nice grey color
       p       <- panel f []
       textlog <- textCtrl p WrapLine [enable := False]

       -- use text control as logger
       textCtrlMakeLogActiveTarget textlog
       logMessage "logging enabled"              

       -- image list
       images  <- imageListCreate (sz 16 16) True 2
       mapM_ (addIcon images) ["../bitmaps/tooltodo.ico","../bitmaps/toolchar.ico"]

       -- list control
       l  <- listCtrlCreate p idAny rectNull wxLC_REPORT
       listCtrlAssignImageList l images wxIMAGE_LIST_SMALL
       listCtrlInsertColumn l 0 "Name" wxLIST_FORMAT_LEFT (-1)
       listCtrlInsertColumn l 0 "Size" wxLIST_FORMAT_LEFT (-1)
       listCtrlInsertColumn l 0 "Date" wxLIST_FORMAT_LEFT (-1)
       mapM_ (\(idx,(name,size,date)) -> do listCtrlInsertItemWithLabel l idx name (-1)
                                            listCtrlSetItem l idx 0 name 0
                                            listCtrlSetItem l idx 1 size 1
                                            listCtrlSetItem l idx 2 date (-1)
                                            return ())
                                            (zip [0..] entries)
       listCtrlOnListEvent l onListEvent 

       -- specify layout
       set f [layout     := container p $ margin 10 $ 
                            column 5 [ fill  $ widget l
                                     , hfill $ widget textlog
                                     ]
             ,clientSize := sz 400 300
             ]
       return ()
  where
    onListEvent eventList
      = case eventList of
          ListItemSelected idx    -> logMessage ("item selected: " ++ show idx)
          ListItemDeselected idx  -> logMessage ("item de-selected: " ++ show idx)
          other                   -> logMessage ("list control event.")

    addIcon images fname
      = do icon <- iconCreateLoad fname (imageTypeFromFileName fname) (sz 16 16)
           imageListAddIcon images icon
           iconDelete icon
           return ()