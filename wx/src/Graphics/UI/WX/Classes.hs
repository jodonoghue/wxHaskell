{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Classes
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Classes
    ( Textual(text,appendText)
    , Dimensions(..)
    , Colored(..)
    , Visible(..)
    , Child(..)
    , Closeable(..)
    , Able( enable )
    , Help( help )
    , Checkable( checkable, checked )
    , Identity( identity )
    , Styled( style )
    , Tipped( tooltip )
    , Selection( selection )
    , Selections( selections )
    ) where

-- for haddock, we import wxh module selectively
-- import Graphics.UI.WXH
import Graphics.UI.WXH.WxcClasses

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout

-- | Widgets with a label or text field.
class Textual w where
  -- | The text of a widget. It is interpreted differently for
  -- for different widgets, for example, the title of a frame or the content of a
  -- static text control.
  text       :: Attr w String
  appendText :: w -> String -> IO ()

  appendText w s
    = set w [text :~ (++s)]


-- | Widgets that have a size.
class Dimensions w where
  -- | The size of a widget (in pixels).
  size      :: Attr w Size
  -- | The (relative) position of a widget.
  position  :: Attr w Point
  -- | The occupied area.
  area      :: Attr w Rect
  -- | The preferred size of a widget.
  bestSize  :: ReadAttr w Size
  -- | The area available for client use (i.e. without the border etc).
  clientSize :: Attr w Size

  -- defaults
  size
    = mapAttr rectSize (\sz r -> rect (topLeft r) sz) area
  position
    = mapAttr topLeft (\pt r -> rect pt (rectSize r)) area
  area
    = newAttr "area" getArea setArea
    where
      getArea w
        = do sz <- get w size
             pt <- get w position
             return (rect pt sz)
      setArea w rect
        = set w [size := rectSize rect, position := topLeft rect]

  clientSize
    = size
  bestSize
    = size

class Colored w where
  -- | The background color.
  bgcolor    :: Attr w Color
  -- | The (foreground) color
  color      :: Attr w Color

  bgcolor
    = nullAttr "bgcolor"
  color
    = nullAttr "color"

-- | Visible widgets.
class Visible w where
  -- | Is the widget visible?
  visible    :: Attr w Bool
  -- | Refresh the widget explicitly.
  refresh    :: w -> IO ()

  -- defaults
  visible
    = nullAttr "visible"
  refresh w
    = return ()

-- | Widgets that are part of a hierarchical order.
class Child w where
  -- | The parent widget.
  parent :: ReadAttr w (Window ())

  -- defaults
  parent
    = readAttr "parent" (\w -> return objectNull)


class Closeable w where
  -- | Close the widget.
  close  :: w -> IO ()


-- | Widgets that can be enabled or disabled.
class Able w where
  -- | Enable, or disable, the widget.
  enable :: Attr w Bool

-- | Widgets with help text.
class Help w where
  -- | Short help text, normally displayed in the status bar or popup balloon.
  help :: Attr w String

-- | Checkable widgets
class Checkable w where
  -- | Is the widget checkable?
  checkable :: Attr w Bool
  -- | Is the widget checked?
  checked   :: Attr w Bool

-- | The identity determines the wxWindows ID of a widget.
class Identity w where
  -- | The identity determines the wxWindows ID of a widget.
  identity :: Attr w Int


-- | The style is a bitmask that determines various properties of a widget.
class Styled w where
  -- | The windows style.
  style :: Attr w Int

-- | Widgets that have a tooltip
class Tipped w where
  -- | The tooltip information
  tooltip :: Attr w String

-- | Widgets with a single selection (radio group or listbox)
class Selection w where
  -- | The current selection as a zero-based index.
  -- Certain widgets return -1 when no item is selected.
  selection :: Attr w Int

-- | Widget with zero or more selections (multi select list boxes)
class Selections w where
  -- | The currently selected items in zero-based indices.
  selections :: Attr w [Int]