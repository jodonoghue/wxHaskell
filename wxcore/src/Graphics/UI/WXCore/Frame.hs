-----------------------------------------------------------------------------------------
{-| Module      :  Frame
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Frame utility functions.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Frame
        ( -- * Frame
          frameCreateTopFrame
        , frameCreateDefault
        , frameSetTopFrame
        , frameDefaultStyle
          -- * Window
        , windowGetRootParent
        , windowGetFrameParent
        , windowGetMousePosition
        , windowGetScreenPosition
          -- * Dialog
        , dialogDefaultStyle
          -- * Status bar
        , statusBarCreateFields
        ) where

import Data.Bits
import Foreign.Marshal.Array
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcClassTypes
import Graphics.UI.WXCore.Types


-- | The default frame style for a normal top-level 'Frame'.
frameDefaultStyle :: Int
frameDefaultStyle
  = wxDEFAULT_FRAME_STYLE .|. wxCLIP_CHILDREN -- .|. wxNO_FULL_REPAINT_ON_RESIZE 

-- | The default frame style for a normal 'Dialog'.
dialogDefaultStyle :: Int
dialogDefaultStyle
  = wxCAPTION .|. wxSYSTEM_MENU .|. wxTAB_TRAVERSAL .|. wxCLIP_CHILDREN .|. wxCLOSE_BOX
    -- .|. wxNO_FULL_REPAINT_ON_RESIZE 

------------------------------------------------------------------------------------------
--
------------------------------------------------------------------------------------------
-- | Create a default frame and make it the top-level window.
frameCreateTopFrame :: String -> IO (Frame ())
frameCreateTopFrame title
  = do frame <- frameCreateDefault title
       frameSetTopFrame frame
       return frame

-- | Set the top-level frame (calls 'cAppSetTopWindow').
frameSetTopFrame :: Frame a -> IO ()
frameSetTopFrame frame
  = wxcAppSetTopWindow frame

-- | Create a  frame with default settings.
frameCreateDefault :: String -> IO (Frame ())
frameCreateDefault title
  = frameCreate objectNull idAny title rectNull frameDefaultStyle

------------------------------------------------------------------------------------------
-- Window
------------------------------------------------------------------------------------------
-- | The parent frame or dialog of a widget.
windowGetFrameParent :: Window a -> IO (Window ())
windowGetFrameParent w
  = if (instanceOf w classFrame || instanceOf w classDialog)
     then return (downcastWindow w)
     else do p <- windowGetParent w
             if (objectIsNull p)
              then return (downcastWindow w)
              else windowGetFrameParent p


-- | The ultimate root parent of the widget.
windowGetRootParent :: Window a -> IO (Window ())
windowGetRootParent w
  = do p <- windowGetParent w
       if (objectIsNull p)
        then return (downcastWindow w)
        else windowGetRootParent p
       


-- | Retrieve the current mouse position relative to the window position.
windowGetMousePosition :: Window a -> IO Point
windowGetMousePosition w
  = do p <- wxcGetMousePosition
       windowScreenToClient2 w p
  
-- | Get the window position relative to the origin of the display.
windowGetScreenPosition :: Window a -> IO Point
windowGetScreenPosition w
  = windowClientToScreen w pointZero


------------------------------------------------------------------------------------------
-- Statusbar
------------------------------------------------------------------------------------------
statusBarCreateFields :: Frame a -> [Int] -> IO (StatusBar ())
statusBarCreateFields parent widths
  = do sb <- frameCreateStatusBar parent (length widths) wxST_SIZEGRIP
       let len = length widths
       if (len <= 1)
        then return sb
        else do withArray (map toCInt widths) (\pwidths -> statusBarSetStatusWidths sb (length widths) pwidths)
                return sb