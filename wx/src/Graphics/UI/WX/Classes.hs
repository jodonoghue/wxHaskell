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
    ( 
      -- * Attributes
      Textual(text,appendText)
    , Literate(font ,fontfamily, fontface, fontencoding,fontsize, fontweight, fontunderline, fontstyle
              ,textcolor,textbgcolor)
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
    , Items( itemcount, item, items, itemadd )
    -- * Types
    , FontInfo(..)
    ) where

-- for haddock, we import wxh module selectively
-- import Graphics.UI.WXH
import Graphics.UI.WXH.WxcClasses
import Graphics.UI.WXH.Draw

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


-- | Widgets with a font.
class Literate w where
  -- | The font of the widget.
  font       :: Attr w FontInfo

  -- | The font size.
  fontsize   :: Attr w Int

  -- | The font weight.
  fontweight :: Attr w FontWeight

  -- | The font family.
  fontfamily :: Attr w FontFamily

  -- | The font style.
  fontstyle  :: Attr w FontStyle

  -- | The font /face/: determines a platform dependent font.
  fontface   :: Attr w String

  -- | Is the font underlined?
  fontunderline :: Attr w Bool

  -- | Font encoding.
  fontencoding  :: Attr w Int

  -- | Text color.
  textcolor  :: Attr w Color

  -- | Text background color
  textbgcolor:: Attr w Color 

  fontsize      = mapAttr fontSize   (\finfo x -> finfo{ fontSize = x }) font
  fontweight    = mapAttr fontWeight (\finfo x -> finfo{ fontWeight = x }) font
  fontfamily    = mapAttr fontFamily (\finfo x -> finfo{ fontFamily = x }) font
  fontstyle     = mapAttr fontStyle  (\finfo x -> finfo{ fontStyle = x }) font
  fontface      = mapAttr fontFace   (\finfo x -> finfo{ fontFace = x }) font
  fontunderline = mapAttr fontUnderline (\finfo x -> finfo{ fontUnderline = x }) font
  fontencoding  = mapAttr fontEncoding  (\finfo x -> finfo{ fontEncoding = x }) font

  

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
    = mapAttr rectSize (\r sz -> rect (topLeft r) sz) area
  position
      = mapAttr topLeft (\r pt -> rect pt (rectSize r)) area
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


-- | Widgets containing certain items (like strings in a listbox)
class Items w a | w -> a where
  -- | Number of items.
  itemcount  :: ReadAttr w Int
  -- | All the items as a list. This attribute might not be writable for some widgets (like radioboxes)
  items  :: Attr w [a]
  -- | An item by zero-based index.
  item   :: Int -> Attr w a
  -- | Add an items. Only valid for writeable items.
  itemadd :: w -> a -> IO ()

  items
    = newAttr "items" getter setter
    where
      getter :: w -> IO [a]
      getter w
        = do n <- get w itemcount
             mapM (\i -> get w (item i)) [0..n-1]

      setter :: w -> [a] -> IO ()
      setter w xs
        = mapM_ (\(i,x) -> set w [item i := x]) (zip [0..] xs)

  itemadd w x
    = do xs <- get w items
         set w [items := xs ++ [x]]