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
module Graphics.UI.WXH.Frame
        ( frameCreateTopFrame
        , frameCreateDefault
        , frameSetTopFrame
        , frameDefaultStyle
        , dialogDefaultStyle
        , statusBarCreateFields
        ) where

import Data.Bits
import Foreign.Marshal.Array
import Graphics.UI.WXH.WxcTypes
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.WxcClasses
import Graphics.UI.WXH.Types


-- | The default frame style for a normal top-level 'Frame'.
frameDefaultStyle :: Int
frameDefaultStyle
  = wxDEFAULT_FRAME_STYLE .|. wxNO_FULL_REPAINT_ON_RESIZE .|. wxCLIP_CHILDREN

-- | The default frame style for a normal 'Dialog'.
dialogDefaultStyle :: Int
dialogDefaultStyle
  = wxCAPTION .|. wxSYSTEM_MENU .|. wxTAB_TRAVERSAL .|. wxNO_FULL_REPAINT_ON_RESIZE .|. wxCLIP_CHILDREN



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