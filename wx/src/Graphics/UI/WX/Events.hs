{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Events
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Events
            ( -- * Event
               Event
             , on
             , mapEvent
             , skipEvent
             -- * Basic events
             -- ** Commanding
             , Commanding, command
             -- ** Reactive
             , Reactive
             , mouse, keyboard
             , closing, idle, resize, focus, activate
             , Paint
             , paint, repaint
             -- * Event filters
             -- ** Mouse filters
             , enter, leave, motion, drag
             , click, unclick, doubleClick
             , clickRight, unclickRight
             -- * Keyboard event filters
             , anyKey, key, charKey
             , enterKey,tabKey,escKey,helpKey
             , delKey,homeKey,endKey
             , pgupKey,pgdownKey
             , downKey,upKey,leftKey,rightKey
             , rebind
            -- * Types
            -- ** Modifiers
            , Modifiers(..)
            , showModifiers
            , noneDown, justShift, justAlt, justControl, justMeta, isNoneDown

            -- ** Mouse events
            , EventMouse (..)
            , showMouse
            , mousePos, mouseModifiers

            -- ** Keyboard events
            , EventKey (..), Key(..)
            , keyKey, keyModifiers, keyPos
            , showKey, showKeyModifiers
             -- * Internal
             , newEvent
            ) where

-- for haddock, we import wxh module selectively
-- import Graphics.UI.WXH
import Graphics.UI.WXH.WxcClasses hiding (Event)
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.Events

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes

{--------------------------------------------------------------------
   Basic events
--------------------------------------------------------------------}
-- | An event for a widget @w@ that expects an event handler of type @a@.
data Event w a  = Event (Attr w a)

-- | Transform an event to an attribute.
on :: Event w a -> Attr w a
on (Event attr)
  = attr

-- | Change the event type.
mapEvent :: (a -> b) -> (b -> a -> a) -> Event w a -> Event w b
mapEvent get set (Event attr)
  = Event (mapAttr get set attr)


-- | Pass the current event on to the default handlers and parent widgets. Always call
-- this method when the event is not processed
skipEvent :: IO ()
skipEvent
  = skipCurrentEvent

{--------------------------------------------------------------------
   Event classes
--------------------------------------------------------------------}
-- | 'Commanding' widgets fire a 'command' event.
class Commanding w where
  -- | A commanding event, for example a button press.
  command :: Event w (IO ())

-- | 'Reactive' widgets are almost all visible widgets on the screen.
class Reactive w where
  mouse     :: Event w (EventMouse -> IO ())
  keyboard  :: Event w (EventKey -> IO ())

  closing   :: Event w (IO ())
  idle      :: Event w (IO Bool)
  resize    :: Event w (IO ())
  focus     :: Event w (Bool -> IO ())
  activate  :: Event w (Bool -> IO ())

-- | 'Paint' widgets can serve as a canvas.
class Paint w where
  paint     :: Event w (DC () -> Rect -> [Rect] -> IO ())
  repaint   :: w -> IO ()

{--------------------------------------------------------------------
   Mouse event filters
--------------------------------------------------------------------}
click :: Reactive w => Event w (Point -> IO ())
click
  = mouseFilter "click" filter
  where
    filter (MouseLeftDown point mod)  = isNoneDown mod
    filter other                      = False

unclick :: Reactive w => Event w (Point -> IO ())
unclick
  = mouseFilter "unclick" filter
  where
    filter (MouseLeftUp point mod)  = isNoneDown mod
    filter other                    = False

doubleClick :: Reactive w => Event w (Point -> IO ())
doubleClick
  = mouseFilter "doubleClick" filter
  where
    filter (MouseLeftDClick point mod) = isNoneDown mod
    filter other                       = False

drag :: Reactive w => Event w (Point -> IO ())
drag
  = mouseFilter "drag" filter
  where
    filter (MouseLeftDrag point mod)  = isNoneDown mod
    filter other                      = False

motion :: Reactive w => Event w (Point -> IO ())
motion
  = mouseFilter "motion" filter
  where
    filter (MouseMotion point mod)  = isNoneDown mod
    filter other                    = False

clickRight :: Reactive w => Event w (Point -> IO ())
clickRight
  = mouseFilter "clickRight" filter
  where
    filter (MouseRightDown point mod)  = isNoneDown mod
    filter other                       = False

unclickRight :: Reactive w => Event w (Point -> IO ())
unclickRight
  = mouseFilter "unclickRight" filter
  where
    filter (MouseRightUp point mod)  = isNoneDown mod
    filter other                     = False

enter :: Reactive w => Event w (Point -> IO ())
enter
  = mouseFilter "enter" filter
  where
    filter (MouseEnter point mod)  = isNoneDown mod
    filter other                   = False

leave :: Reactive w => Event w (Point -> IO ())
leave
  = mouseFilter "leave" filter
  where
    filter (MouseLeave point mod)  = isNoneDown mod
    filter other                   = False

mouseFilter :: Reactive w => String -> (EventMouse -> Bool) -> Event w (Point -> IO ())
mouseFilter name filter
  = mapEvent get set mouse
  where
    get prev x
      = ioError (userError ("WX.Events: the " ++ name ++ " event is write-only."))

    set new prev mouseEvent
      = do when (filter mouseEvent) (new (mousePos mouseEvent))
           prev mouseEvent

{--------------------------------------------------------------------
  Keyboard filter events
--------------------------------------------------------------------}
rebind :: Event w (IO ()) -> Event w (IO ())
rebind event
  = mapEvent get set event
  where
    get prev
      = prev

    set new prev
      = new

enterKey,tabKey,escKey,helpKey,delKey,homeKey,endKey :: Reactive w => Event w (IO ())
pgupKey,pgdownKey,downKey,upKey,leftKey,rightKey :: Reactive w => Event w (IO ())
enterKey  = key KeyReturn
tabKey    = key KeyTab
escKey    = key KeyEscape
helpKey   = key KeyHelp
delKey    = key KeyDelete
homeKey   = key KeyHome
endKey    = key KeyEnd
pgupKey   = key KeyPrior
pgdownKey = key KeyNext
downKey   = key KeyDown
upKey     = key KeyUp
leftKey   = key KeyLeft
rightKey  = key KeyRight

charKey :: Reactive w => Char -> Event w (IO ())
charKey c
  = key (KeyChar c)

key :: Reactive w => Key -> Event w (IO ())
key k
  = keyboardFilter "key" filter
  where
    filter (EventKey x mod pt)  = k==x


anyKey :: Reactive w => Event w (Key -> IO ())
anyKey
  = keyboardFilter1 "anyKey" (const True)


keyboardFilter :: Reactive w => String -> (EventKey -> Bool) -> Event w (IO ())
keyboardFilter name filter
  = mapEvent get set keyboard
  where
    get prev
      = ioError (userError ("WX.Events: the " ++ name ++ " event is write-only."))

    set new prev keyboardEvent
      = do when (filter keyboardEvent) new
           prev keyboardEvent

keyboardFilter1 :: Reactive w => String -> (EventKey -> Bool) -> Event w (Key -> IO ())
keyboardFilter1 name filter
  = mapEvent get set keyboard
  where
    get prev key
      = ioError (userError ("WX.Events: the " ++ name ++ " event is write-only."))

    set new prev keyboardEvent
      = do when(filter keyboardEvent) (new (keyKey keyboardEvent))
           prev keyboardEvent



{--------------------------------------------------------------------
   Generic event creators
-------------------------------------------------------------------}
-- | Create a new event from a get and set function.
newEvent :: String -> (w -> IO a) -> (w -> a -> IO ()) -> Event w a
newEvent name getter setter
  = Event (newAttr name getter setter)