{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Window
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Exports default instances for generic windows.

    * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.             
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Window
        ( -- * Window
          Window, refit, refitMinimal, rootParent, frameParent
        , resizeable, maximizeable, minimizeable, clipChildren
          -- * ScrolledWindow
        , ScrolledWindow, scrolledWindow, scrollRate
          -- * Internal
        , fullRepaintOnResizeFlags, resizeableFlags, maximizeableFlags, minimizeableFlags, clipChildrenFlags
        ) where

import Graphics.UI.WXCore

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events

{--------------------------------------------------------------------------------
  ScrolledWindow
--------------------------------------------------------------------------------}
-- | A scrollable window. Use 'virtualSize' and 'scrollRate' to set the scrollbar
-- behaviour.
--
-- * Attributes: 'scrollRate'
--
-- * Instances: 'HasImage', 'Form', 'Closable' -- 
--             'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
scrolledWindow :: Window a -> [Prop (ScrolledWindow ())] -> IO (ScrolledWindow ())
scrolledWindow parent props
  = do sw <- scrolledWindowCreate parent idAny rectNull 
              (clipChildrenFlags props (fullRepaintOnResizeFlags props 0))
       set sw props
       return sw


-- | The horizontal and vertical scroll rate of scrolled window. Use @0@ to disable 
-- scrolling in that direction.
scrollRate :: Attr (ScrolledWindow a) Size
scrollRate
  = newAttr "scrollRate" getter setter
  where
    getter sw
      = do p <- scrolledWindowGetScrollPixelsPerUnit sw
           return (sizeFromPoint p)
     
    setter sw size
      = scrolledWindowSetScrollRate sw (sizeW size) (sizeH size)

{--------------------------------------------------------------------------------
  Properties
--------------------------------------------------------------------------------}
instance Able (Window a) where
  enabled
    = newAttr "enabled" windowIsEnabled setter
    where
      setter w enable
        | enable    = unitIO $ windowEnable w
        | otherwise = unitIO $ windowDisable w


instance Textual (Window a) where
  text
    = newAttr "text" getter setter
    where
      getter w
        = fst (getset w)
      setter w x
        = snd (getset w) x

      getset w
        = ifInstanceOf w classComboBox
            (\cb -> (comboBoxGetValue cb, \s -> do comboBoxClear cb; comboBoxAppend cb s)) $
          ifInstanceOf w classTextCtrl
            (\tc -> (textCtrlGetValue tc, \s -> do textCtrlClear tc; textCtrlWriteText tc s)) $
            (windowGetLabel w,windowSetLabel w)

  appendText w s
    = ifInstanceOf w classComboBox
        (\cb -> comboBoxAppend cb s) $
      ifInstanceOf w classTextCtrl
        (\tc -> textCtrlAppendText tc s)
        (set w [text :~ (++s)])


instance Dimensions (Window a) where
  outerSize
    = newAttr "size" windowGetSize setSize
    where
      setSize w sz
        = windowSetSize w (rect (pt (-1) (-1)) sz) wxSIZE_USE_EXISTING

  area
    = newAttr "area" windowGetRect setArea
    where
      setArea w rect
        = windowSetSize w rect wxSIZE_USE_EXISTING

  bestSize
    = readAttr "bestSize" windowGetAdjustedBestSize

  position
    = newAttr "position" windowGetPosition windowMove

  clientSize
    = newAttr "clientSize" windowGetClientSize windowSetClientSize

  virtualSize
    = newAttr "virtualSize" windowGetVirtualSize windowSetVirtualSize


instance Colored (Window a) where
  bgcolor
    = newAttr "bgcolor" windowGetBackgroundColour (\w x -> do{ windowSetBackgroundColour w x; return ()})

  color
    = newAttr "color" windowGetForegroundColour (\w x -> do windowSetForegroundColour w x; return ())


instance Literate (Window a) where
  font
    = newAttr "font" getter setter
    where
      getter w
        = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textCtrlGetDefaultStyle textCtrl)
                                 (textAttrDelete)
                                 (\attr -> do hasFont <- textAttrHasFont attr
                                              if (hasFont) 
                                                then getFont (textAttrGetFont attr) 
                                                else getFont (windowGetFont w)))
           (getFont (windowGetFont w))
        where
          getFont io
            = bracket io fontDelete fontGetFontStyle 

      setter w info
        = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textAttrCreateDefault)
                                 (textAttrDelete)
                                 (\attr -> withFontStyle info $ \fnt ->
                                           do textAttrSetFont attr fnt
                                              textCtrlSetDefaultStyle textCtrl attr
                                              return ()))
           (withFontStyle info $ \fnt ->
            do windowSetFont w fnt
               return ())

  textColor
    = newAttr "textcolor" getter setter
    where
      getter w
        = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textCtrlGetDefaultStyle textCtrl)
                                 (textAttrDelete)
                                 (\attr -> do hasColor <- textAttrHasTextColour attr
                                              if (hasColor) then textAttrGetTextColour attr
                                                            else get w color))
           (get w color)

      setter w c
        = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textAttrCreateDefault)
                                 (textAttrDelete)
                                 (\attr -> do textAttrSetTextColour attr c
                                              textCtrlSetDefaultStyle textCtrl attr
                                              return ()))
           (set w [color := c])

  textBgcolor
    = newAttr "textbgcolor" getter setter
    where
      getter w
        = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textCtrlGetDefaultStyle textCtrl)
                                 (textAttrDelete)
                                 (\attr -> do hasColor <- textAttrHasBackgroundColour attr
                                              if (hasColor) then textAttrGetBackgroundColour attr
                                                            else get w bgcolor))
           (get w bgcolor)

      setter w c
       = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textAttrCreateDefault)
                                 (textAttrDelete)
                                 (\attr -> do textAttrSetBackgroundColour attr c
                                              textCtrlSetDefaultStyle textCtrl attr
                                              return ()))
           (set w [bgcolor := c])

  

instance Visible (Window a) where
  visible
    = newAttr "visible" windowIsShown setVisible
    where
      setVisible w vis
        = if vis
           then do{ windowShow w; windowRaise w }
           else unitIO (windowHide w)

  refresh w
    = windowRefresh w False

  fullRepaintOnResize
    = reflectiveAttr "fullRepaintOnResize" getFlag setFlag
    where
      getFlag w
        = do s <- get w style
             return (not (bitsSet wxNO_FULL_REPAINT_ON_RESIZE s))

      setFlag w repaint
        = set w [style :~ \stl -> if repaint 
                                   then stl .-. wxNO_FULL_REPAINT_ON_RESIZE 
                                   else stl .+. wxNO_FULL_REPAINT_ON_RESIZE]


-- | Helper function that transforms the style according to the 'fullRepaintOnResize' flags.
fullRepaintOnResizeFlags :: Visible w => [Prop w] -> Int -> Int
fullRepaintOnResizeFlags props stl
  = case getPropValue fullRepaintOnResize props of
      Just True  -> stl .-. wxNO_FULL_REPAINT_ON_RESIZE
      Just False -> stl .+. wxNO_FULL_REPAINT_ON_RESIZE
      other      -> stl



-- | Display a resize border on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
resizeable :: Attr (Window a) Bool
resizeable
  = reflectiveAttr "resizeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxRESIZE_BORDER s)
    setFlag w resize
      = set w [style :~ \stl -> if resize 
                                 then stl .+. wxRESIZE_BORDER
                                 else stl .-. wxRESIZE_BORDER]

-- | Helper function that transforms the style accordding
-- to the 'resizeable' flag out of the properties
resizeableFlags :: [Prop (Window a)] -> Int -> Int
resizeableFlags props stl
  = case getPropValue resizeable props of
      Just True  -> stl .+. wxRESIZE_BORDER
      Just False -> stl .-. wxRESIZE_BORDER
      Nothing    -> stl



-- | Clip the children of window during drawing ('wxCLIP_CHILDREN' flag).
-- This attribute must be set at creation time.
clipChildren :: Attr (Window a) Bool
clipChildren
  = reflectiveAttr "clipChildren" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxCLIP_CHILDREN s)
    setFlag w clip
      = set w [style :~ \stl -> if clip
                                 then stl .+. wxCLIP_CHILDREN
                                 else stl .-. wxCLIP_CHILDREN]

-- | Helper function that transforms the style accordding
-- to the 'clipChildren' flag out of the properties
clipChildrenFlags :: [Prop (Window a)] -> Int -> Int
clipChildrenFlags props stl
  = case getPropValue clipChildren props of
      Just True -> stl .+. wxCLIP_CHILDREN
      Just False-> stl .-. wxCLIP_CHILDREN
      Nothing   -> stl




-- | Display a maximize box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
maximizeable :: Attr (Window a) Bool
maximizeable
  = reflectiveAttr "maximizeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxMAXIMIZE_BOX s)
    setFlag w max
      = set w [style :~ \stl -> if max then stl .+. wxMAXIMIZE_BOX else stl .-. wxMAXIMIZE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'maximizable' flag out of the properties
maximizeableFlags :: [Prop (Window a)] -> Int -> Int
maximizeableFlags props stl
  = case getPropValue maximizeable props of
      Just True  -> stl .+. wxMAXIMIZE_BOX
      Just False -> stl .-. wxMAXIMIZE_BOX
      Nothing    -> stl


-- | Display a minimize box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
minimizeable :: Attr (Window a) Bool
minimizeable
  = reflectiveAttr "minimizeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxMINIMIZE_BOX s)
    setFlag w min
      = set w [style :~ \stl -> if min then stl .+. wxMINIMIZE_BOX else stl .-. wxMINIMIZE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'minimizable' flag out of the properties
minimizeableFlags :: [Prop (Window a)] -> Int -> Int
minimizeableFlags props stl
  = case getPropValue minimizeable props of
      Just True  -> stl .+. wxMINIMIZE_BOX
      Just False -> stl .-. wxMINIMIZE_BOX
      Nothing    -> stl


instance Child (Window a) where
  parent
    = readAttr "parent" windowGetParent

-- | Ensure that a widget is refitted inside a window when
-- its size changes, for example when the 'text' of a 
-- 'staticText' control changes. (calls 'windowReFit')
refit :: Window a -> IO ()
refit w
  = windowReFit w

-- | Ensure that a widget is refitted inside a window when
-- its size changes, for example when the 'text' of a 
-- 'staticText' control changes. Always resizes the
-- window to its minimal acceptable size. (calls 'windowReFitMinimal')
refitMinimal :: Window a -> IO ()
refitMinimal w
  = windowReFitMinimal w

-- | The ultimate root parent of the widget.
rootParent :: ReadAttr (Window a) (Window ())
rootParent 
  = readAttr "rootParent" windowGetRootParent 

-- | The parent frame or dialog of a widget.
frameParent :: ReadAttr (Window a) (Window ())
frameParent 
  = readAttr "frameParent" windowGetFrameParent 


instance Identity (Window a) where
  identity
    = newAttr "identity" windowGetId windowSetId

instance Styled (Window a) where
  style
    = newAttr "style" windowGetWindowStyleFlag windowSetWindowStyleFlag

instance Tipped (Window a) where
  tooltip
    = newAttr "tooltip" windowGetToolTip windowSetToolTip

{-
instance Help (Window a) where
  help
    = newAttr "help" windowSetHelpText windowGetHelpText
-}

{--------------------------------------------------------------------------------
  Events
--------------------------------------------------------------------------------}
instance Reactive (Window a) where
  mouse     = newEvent "mouse" windowGetOnMouse (\w h -> windowOnMouse w True h)
  keyboard  = newEvent "keyboard" windowGetOnKeyChar (windowOnKeyChar)
  closing   = newEvent "closing" windowGetOnClose windowOnClose
  idle      = newEvent "idle" windowGetOnIdle windowOnIdle
  resize    = newEvent "resize" windowGetOnSize windowOnSize
  focus     = newEvent "focus" windowGetOnFocus windowOnFocus
  activate  = newEvent "activate" windowGetOnActivate windowOnActivate

instance Paint (Window a) where
  paint     = newEvent "paint" windowGetOnPaint (\w h -> windowOnPaint w h)
  paintRaw  = newEvent "paintRaw" windowGetOnPaintRaw (\w h -> windowOnPaintRaw w h)
  repaint w = windowRefresh w False