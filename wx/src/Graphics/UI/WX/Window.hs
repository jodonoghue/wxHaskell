{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Window
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Exports default instances for generic windows.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Window
        ( -- * Window
          Window 
          -- * ScrolledWindow
        , ScrolledWindow, scrolledWindow
        ) where

import Graphics.UI.WXH
import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events

{--------------------------------------------------------------------------------
  ScrolledWindow
--------------------------------------------------------------------------------}
scrolledWindow :: Window a -> [Prop (ScrolledWindow ())] -> IO (ScrolledWindow ())
scrolledWindow parent props
  = do sw <- scrolledWindowCreate parent idAny rectNull 0
       set sw props
       return sw



{--------------------------------------------------------------------------------
  Properties
--------------------------------------------------------------------------------}
instance Able (Window a) where
  enable
    = newAttr "enable" windowIsEnabled setter
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
  size
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
    = readAttr "bestSize" windowGetBestSize

  position
    = newAttr "position" windowGetPosition windowMove

  clientSize
    = newAttr "clientSize" windowGetClientSize windowSetClientSize


instance Colored (Window a) where
  bgcolor
    = newAttr "bgcolor" windowGetBackgroundColour (\w x -> do{ windowSetBackgroundColour w x; return ()})

  color
    = newAttr "color" getter setter
    where
      getter w
        = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textCtrlGetDefaultStyle textCtrl) 
                                 (textAttrDelete)
                                 (textAttrGetTextColour))
           (windowGetForegroundColour w)
      setter w clr
        = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textAttrCreateDefault)
                                 (textAttrDelete)
                                 (\attr -> do textAttrSetTextColour attr clr
                                              textCtrlSetDefaultStyle textCtrl attr
                                              return ()))
           (unitIO (windowSetForegroundColour w clr))


instance Literate (Window a) where
  font
    = newAttr "font" getter setter
    where
      getter w
        = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textCtrlGetDefaultStyle textCtrl) 
                                 (textAttrDelete)
                                 (\attr -> bracket (textAttrGetFont attr) fontDelete fontGetFontInfo))
           (bracket (windowGetFont w) fontDelete fontGetFontInfo)

      setter w info
        = ifInstanceOf w classTextCtrl
           (\textCtrl -> bracket (textAttrCreateDefault)
                                 (textAttrDelete)
                                 (\attr -> withFontInfo info $ \fnt ->
                                           do textAttrSetFont attr fnt
                                              textCtrlSetDefaultStyle textCtrl attr
                                              return ()))
           (withFontInfo info (unitIO . windowSetFont w))


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


instance Child (Window a) where
  parent
    = readAttr "parent" windowGetParent

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
  paint     = newEvent "paint" windowGetOnPaint (\w h -> windowOnPaint w True h)
  repaint w = windowRefresh w False