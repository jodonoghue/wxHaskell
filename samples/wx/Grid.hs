{--------------------------------------------------------------------------------
   Test Grid.
--------------------------------------------------------------------------------}
module Main where
 
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Event)

main  
  = start gui

gui :: IO ()
gui 
  = do f <- frame [text := "Grid test", visible := False] 
           
       -- use text control as logger
       textlog <- textCtrl f [wrap := WrapNone, enabled := False] 
       textCtrlMakeLogActiveTarget textlog
       logMessage "logging enabled"              

       -- grids
       g <- gridCtrl f []
       gridSetGridLineColour g (colorSystem Color3DFace)
       gridSetCellHighlightColour g black
       appendColumns g (head names)
       appendRows    g (map show [1..length (tail names)])
       mapM_ (setRow g) (zip [0..] (tail names))
       gridAutoSize g

       windowOnKeyDown g (onGridKeyDown g)
       set g [on gridEvent := onGrid]

       -- layout
       set f [layout := column 5 [fill (dynamic (widget g))
                                 ,hfill $ minsize (sz 20 80) $ widget textlog]
             ]       
       focusOn g
       set f [visible := True]  -- reduce flicker at startup.
       return ()
  where
    onGridKeyDown g (EventKey key mods pt)
      = case key of
          KeyReturn ->          
            do logMessage "keyEnter"
               gridMoveNext g
          _ -> propagateEvent

    onGrid ev
      = case ev of
          GridCellChange row col veto
            -> logMessage ("cell changed: " ++ show (row,col))
          _ -> propagateEvent
names
  = [["First Name", "Last Name"]
    ,["Daan","Leijen"],["Arjan","van IJzendoorn"]
    ,["Martijn","Schrage"],["Andres","Loh"]]


setRow g (row,values)
  = mapM_ (\(col,value) -> gridSetCellValue g row col value) (zip [0..] values)


{--------------------------------------------------------------------------------
   Library?
--------------------------------------------------------------------------------}

gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
gridCtrl parent props
  = feed2 props 0 $
    initialWindow $ \id rect -> \props flags ->
    do g <- gridCreate parent id rect flags
       gridCreateGrid g 0 0 0
       set g props
       return g

gridEvent :: Event (Grid a) (EventGrid -> IO ())
gridEvent
  = newEvent "gridEvent" gridGetOnGridEvent gridOnGridEvent


gridMoveNext :: Grid a -> IO ()
gridMoveNext g
  = do row <- gridGetGridCursorRow g
       col <- gridGetGridCursorCol g
       rowCount <- gridGetNumberRows g
       colCount <- gridGetNumberCols g
       let (r,c) = if (row+1 >= rowCount)
                    then if (col+1 >= colCount)
                     then (0,0)
                     else (0,col+1)
                    else (row+1,col)
       gridSetGridCursor g r c
       gridMakeCellVisible g r c
       return ()


appendColumns :: Grid a -> [String] -> IO ()
appendColumns g []
  = return ()
appendColumns g labels
  = do n <- gridGetNumberCols g
       gridAppendCols g (length labels) True
       mapM_ (\(i,label) -> gridSetColLabelValue g i label) (zip [n..] labels)

appendRows :: Grid a -> [String] -> IO ()
appendRows g []
  = return ()
appendRows g labels
  = do n <- gridGetNumberRows g
       gridAppendRows g (length labels) True
       mapM_ (\(i,label) -> gridSetRowLabelValue g i label) (zip [n..] labels)

