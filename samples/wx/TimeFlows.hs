{--------------------------------------------------------------------------------
  Copyright (c) 2003 Daan Leijen.

  "Time flows like a river" -- an old Fran demo :-)

  Demonstrates the use of an idle event handler to implement a resource
  aware gui that still does heavy animation.
--------------------------------------------------------------------------------}
module Main where

import System.CPUTime
import Graphics.UI.WXH (getTextExtent)
import Graphics.UI.WX

{-------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------}
-- Time is in seconds, represented as a double.
type Time = Double

-- The (mouse) history consists of time/position pairs (and is never null)
type History = [(Time,Point)]


{-------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------}
-- The total time lag of the words behind the mouse cursor
timeSpan :: Time
timeSpan = 1

-- The flowing text
flowText = "Time flows like a river"

-- The font style
flowFont = fontSwiss{ _fontSize = 16 }


{-------------------------------------------------------------------------
  The gui
-------------------------------------------------------------------------}
main
  = start timeFlows

timeFlows
  = do -- mouse history as list of time/position pairs: is never null!
       vmouseHistory <- varCreate [(0,pt 0 0)]

       -- create a frame.
       f <- frame   [ text        := flowText 
                    , clientSize  := sz 300 300
                    ]

       -- set event handlers
       set f        [ on paint    := onPaint  vmouseHistory flowText
                    , on idle     := onIdle   vmouseHistory f
                    , on drag     := onDrag   vmouseHistory
                    ]
       return ()

{-------------------------------------------------------------------------
  Event handlers
-------------------------------------------------------------------------}
-- repaint handler
onPaint vmouseHistory text dc viewRect updateAreas
  = do history <- varGet vmouseHistory
       time    <- getTime
       -- draw trace line
       polyline dc (map snd history) [penColor := lightgrey]
       -- draw the words
       set dc [font := fontSwiss{ _fontSize = 16} ]
       mapM_ drawWord (wordPositions history (wordTimes timeSpan time (words text)))
  where
    drawWord (pos,word)
      = do -- center word
           sz <- getTextExtent dc word
           let newX = pointX pos - (sizeW sz `div` 2)
               newY = pointY pos - (sizeH sz `div` 2)
           -- and draw it.
           drawText dc word (pt newX newY) []

           
-- idle event handler
onIdle :: Var History -> Window a -> IO Bool
onIdle vmouseHistory win
  = do hist <- varGet vmouseHistory
       if (length hist <= 1)
        then do -- don't call idle again until some other event happens
                return False
        else do time <- getTime
                repaint win
                -- prune the history  
                let (recent,old) = splitAfter (time-timeSpan) [] hist  
                varSet vmouseHistory recent
                -- keep calling idle only when there is something to do
                return (not (null old)) 
  where
    -- split the history in two lists at a certain time. 
    -- but ensure that the first list is never empty.
    splitAfter time acc []     = (reverse acc,[])
    splitAfter time acc (x@(t,pos):xs) 
      | t >= time  = splitAfter time (x:acc) xs
      | null acc   = splitAfter time (x:acc) xs
      | otherwise  = (reverse acc,x:xs)

-- mouse drag handler
onDrag vmouseHistory mousePos
  = do time <- getTime
       varUpdate vmouseHistory ((time,mousePos):)
       return ()
           

{-------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------}

-- Translate time/word pairs to position/word pairs given the mouse
-- position history.
wordPositions :: History -> [(Time,String)] -> [(Point,String)]
wordPositions history timedWords
  = [(posAtTime t history, word) | (t,word) <- timedWords]

-- | Return the mouse position at a certain time.
posAtTime :: Time -> History -> Point
posAtTime time [(t,pos)]    = pos
posAtTime time ((t,pos):xs) | t <= time  = pos
                            | otherwise  = posAtTime time xs

-- | Assign times to the words in a string, given a timeSpan and current time.
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


