{--------------------------------------------------------------------------------
  Bouncing Balls demo
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WXCore

main
  = run ballsFrame

ballsFrame
  = do -- a list of balls, where each ball is represented by a list of all future Y positions.
       vballs <- varCreate []

       -- create a non-user-resizable top-level (orphan) frame.
       f <- frameCreate objectNull idAny "Bouncing balls" rectNull
                         ( wxMINIMIZE_BOX + wxSYSTEM_MENU + wxCAPTION + wxNO_FULL_REPAINT_ON_RESIZE
                         + wxCLIP_CHILDREN + wxCLOSE_BOX)

       -- add a panel to draw on, nice grey color.
       p <- panelCreate f idAny rectNull 0 -- (wxNO_FULL_REPAINT_ON_RESIZE)
       let instructions = init . unlines $
                                  [ "Click to create more bouncing balls"
                                  , "Right-click to for a new window"
                                  , "<+/-> to change the speed" ]
       windowSetBackgroundColour f $ colorSystem Color3DFace
       windowSetLayout f (column 1 [ minsize (sz maxX maxY) (widget p)
                                   , label instructions ])

       -- create a timer, on each tick it advances all the balls to their next position
       t <- windowTimerCreate f
       timerOnCommand t (nextBalls p vballs)

       -- paint the balls unbuffered
       windowOnPaintRaw p (paintBalls vballs)

       -- left-click: new ball, right-click: new window
       windowOnMouse p False {- no motion events -} (onMouse p vballs)

       -- '-': decrease interval, '+': increase interval.
       windowOnKeyChar p (onKey t)

       -- show the frame
       windowShow f
       windowRaise f

       -- and start the timer (25 msec).
       timerStart t 25 False {- one-shot timer? -}
       return ()
  where
    -- react on mouse events
    onMouse w vballs mouse
      = case mouse of
          MouseLeftDown pt mods  -> dropBall w vballs pt  -- new ball
          MouseRightDown pt mods -> ballsFrame            -- new window with bouncing balls
          other                  -> skipCurrentEvent      -- unprocessed event: send up the window chain

    -- react on the keyboard
    onKey t keyboard
      = case keyKey keyboard of
          KeyChar '-'   -> updateInterval t (\i -> i+5)
          KeyChar '+'   -> updateInterval t (\i -> max 1 (i-5))
          other         -> skipCurrentEvent

    updateInterval t f
      = do i <- timerGetInterval t
           timerStop t
           timerStart t (f i) False
           return ()

    -- advance all the balls to their next position
    nextBalls w vballs
      = do varUpdate vballs (filter (not.null) . map (drop 1))
           windowRefresh w False

    -- add a new ball
    dropBall w vballs pt
      = do varUpdate vballs (bouncing pt:)
           windowRefresh w False

    -- calculate all future positions
    bouncing (Point x y)
      = map (\h -> Point x (maxH-h)) (bounce (maxH-y) 0)

    bounce h v
      | h <= 0 && v == 0     = []
      | h <= 0 && v  < 0     = bounce 0 ((-v)-2)
      | otherwise            = h : bounce (h+v) (v-1)


    -- paint the balls
    paintBalls vballs dc viewRect updateAreas
      = do dcClear dc
           balls <- varGet vballs
           dcWithBrushStyle dc (BrushStyle BrushSolid red) $
             mapM_ (drawBall dc) (map head (filter (not.null) balls))

    drawBall dc pt
      = dcDrawCircle dc pt radius


-- radius the ball, and the maximal x and y coordinates
radius, maxX, maxY :: Int
maxY   = 300
maxX   = 300
radius = 10

-- the max. height is at most max. y minus the radius of a ball.
maxH :: Int
maxH   = maxY - radius