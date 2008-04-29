{--------------------------------------------------------------------------------
  Copyright (c) 2003 Daan Leijen.

  "Time flows like a river" -- an old Fran demo :-)

  Demonstrates the use of an idle event handler to implement a resource
  aware gui that still does heavy animation.

  This is an extended version of the "TimeFlows" demo that adds menus
  and dialogs to customize the appearance of the text, font, and delay.
--------------------------------------------------------------------------------}
module Main where

import System.CPUTime
import Graphics.UI.WXCore hiding (Time)-- (getTextExtent)
import Graphics.UI.WX

{-------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------}
-- Time is in seconds, represented as a double.
type Time = Double

-- The (mouse) history consists of time/position pairs (and is never null)
type History = [(Time,Point)]


{-------------------------------------------------------------------------
  The gui
-------------------------------------------------------------------------}
main
  = start timeFlows

timeFlows
  = do -- mouse history as list of time/position pairs: is never null!
       vmouseHistory <- varCreate [(0,pt 0 0)]
       -- the total time delay of the last word (in seconds)
       vtimeSpan     <- varCreate 1.0
       -- the text
       vflowText     <- varCreate "time flows like a river"
       -- the font
       vflowFont     <- varCreate (fontSwiss{ _fontSize = 16 })
       -- show mouse track?
       vflowLine     <- varCreate True

       -- create a frame and panel to draw in.
       f <- frame   [ text        := "Time flows"]
       p <- panel f []

       -- create menus & status fields       
       mfile    <- menuPane       [text := "&File"]
       mquit    <- menuQuit mfile [help := "Exit the demo", on command := close f]

       medit    <- menuPane       [text := "&Edit"]
       mshowline<- menuItem medit [text := "&Show mouse track\tCtrl+S", checkable := True, checked := True]
       mfont    <- menuItem medit [text := "&Font...\tCtrl+F", help := "Set the font"]
       moptions <- menuItem medit [text := "&Options...\tCtrl+O", help := "Edit demo options"]
       
       mhelp    <- menuHelp        []
       mabout   <- menuAbout mhelp [help := "Information about this demo."]

       flowText <- varGet vflowText
       status   <- statusField [text := flowText]

       -- set layout (before the menubar!)
       set f [ layout      := fill $ widget p
             , clientSize  := sz 300 300        --initial size
             ]

       -- set menu and status bar
       set f [ menuBar     := [mfile,medit,mhelp]
             , statusBar   := [status]
             , on (menu mabout)    := infoDialog f "About Time flows.." "This is an idle event application."
             , on (menu mshowline) := do showit <- get mshowline checked                                 
                                         varSet vflowLine showit
             , on (menu moptions)  := showOptionDialog f vtimeSpan vflowText status
             , on (menu mfont)     := showFontDialog f vflowFont
             , on (charKey '+')    := do varUpdate vtimeSpan (\n -> min 10 (n+1)); return ()
             , on (charKey '-')    := do varUpdate vtimeSpan (\n -> max  1 (n-1)); return ()
             ]
      
       -- set event handlers
       set p [ on paint    := onPaint  vmouseHistory vtimeSpan vflowLine vflowText vflowFont
             , on idle     := onIdle   vmouseHistory vtimeSpan p
             , on drag     := onDrag   vmouseHistory
             ]

       return ()

{-------------------------------------------------------------------------
  Dialogs
-------------------------------------------------------------------------}
showFontDialog frame vflowFont
  = do flowFont <- varGet vflowFont
       mbfont   <- fontDialog frame flowFont
       case mbfont of
         Nothing   -> return ()
         Just font -> varSet vflowFont font

showOptionDialog frame vtimeSpan vflowText status
  = do flowText <- varGet vflowText
       timeSpan <- varGet vtimeSpan
      
       -- create dialog
       d     <- dialog frame [text := "Options", resizeable := True]
       p     <- panel d []
       entry <- textEntry p [text := flowText]
       delay <- spinCtrl  p 1 10 [selection := round timeSpan]
       ok    <- button p [text := "Ok"] 
       can   <- button p [text := "Cancel"]           
       
       -- layout
       set d [defaultButton := ok
             ,layout := container p $ margin 10 $ 
                        column 10 [ boxed "" $ grid 5 5 [[label "text:",  hfill $ widget entry]
                                                        ,[label "delay:", floatRight $ widget delay]
                                                        ]
                                  , floatBottomRight $ row 5 [widget ok, widget can]
                                  ]
             ]

       -- show modal 
       ret   <- showModal d $ \stop -> 
                do set ok  [on command := do flowText <- get entry text 
                                             timeSpan <- get delay selection
                                             stop (Just (flowText,fromIntegral timeSpan))]
                   set can [on command := stop Nothing]
                   
       -- set results
       case ret of
         Nothing -> return ()
         Just (flowText,timeSpan)
                 -> do varSet vflowText flowText
                       set status [text := flowText]
                       varSet vtimeSpan timeSpan


{-------------------------------------------------------------------------
  Event handlers
-------------------------------------------------------------------------}
-- repaint handler
onPaint vmouseHistory vtimeSpan vflowLine vflowText vflowFont  dc viewArea
  = do time     <- getTime
       history  <- varGet vmouseHistory
       timeSpan <- varGet vtimeSpan
       flowFont <- varGet vflowFont
       flowText <- varGet vflowText
       flowLine <- varGet vflowLine

       -- draw trace line
       when (flowLine) (polyline dc (map snd history) [penColor := lightgrey])
       -- draw the words
       set dc [font := flowFont ]
       mapM_ drawWord (wordPositions history timeSpan time flowText)
  where
    drawWord (pos,word)
      = do -- center word
           sz <- getTextExtent dc word
           let newX = pointX pos - (sizeW sz `div` 2)
               newY = pointY pos - (sizeH sz `div` 2)
           -- and draw it.
           drawText dc word (pt newX newY) []

           
-- idle event handler
onIdle :: Var History -> Var Time -> Window a -> IO Bool
onIdle vmouseHistory vtimeSpan win
  = do history <- varGet vmouseHistory
       if (null (tail history))
        then do -- don't call idle again until some other event happens
                return False
        else do time     <- getTime
                timeSpan <- varGet vtimeSpan
                repaint win
                -- prune the history  
                varSet vmouseHistory (prune (time - timeSpan) history)
                return True
  where
    -- prune the history: only remember time/position pairs up to a certain time span.
    prune time (h:hs)
      = h:takeWhile (after time) hs

    after time (t,p)
      = time <= t


-- mouse drag handler
onDrag vmouseHistory mousePos
  = do time <- getTime
       -- prepend a new time/position pair
       varUpdate vmouseHistory ((time,mousePos):)
       return ()
           

{-------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------}
-- Tuple each word in a string with its historic position, given a mouse
-- history, a time span, and current time.
wordPositions :: History -> Time -> Time -> String -> [(Point,String)]
wordPositions history timeSpan time 
  = wordPositionsAt history . wordTimes timeSpan time . words 

-- Translate time/word pairs to position/word pairs given the mouse position history.
wordPositionsAt :: History -> [(Time,String)] -> [(Point,String)]
wordPositionsAt history timedWords
  = [(posAtTime t history, word) | (t,word) <- timedWords]

-- | Return the mouse position at a certain time.
posAtTime :: Time -> History -> Point
posAtTime time [(t,pos)]    = pos
posAtTime time ((t,pos):xs) | t <= time  = pos
                            | otherwise  = posAtTime time xs

-- | Evenly assign times to the words in a string, given a timeSpan and current time.
wordTimes :: Time -> Time -> [String] -> [(Time,String)]
wordTimes timeSpan time words
  = let n     = length words
        delta = timeSpan / (fromIntegral n)
    in zip (iterate (\t -> t-delta) time) words
    
-- Get the current Time
getTime :: IO Time
getTime
  = do picoSecs <- getCPUTime
       let time = (fromIntegral picoSecs) / 1.0e12
       return time
