{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Frame
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Frame
    ( Frame, frame, frameFixed, frameTool, frameEx, image
    ) where

-- for haddock, we import wxh module selectively
-- import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.Frame

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Window

-- | Create a top-level frame window.
frame :: [Prop (Frame ())]  -> IO (Frame ())
frame props
  = frameEx frameDefaultStyle props objectNull

-- | Create a top-level frame window that is not resizeable.
frameFixed :: [Prop (Frame ())]  -> IO (Frame ())
frameFixed props
  = frameEx (frameDefaultStyle .-. wxMAXIMIZE_BOX .-. wxRESIZE_BORDER)  props objectNull

-- | Create a tool window; floats on the parent and has a small caption.
frameTool :: [Prop (Frame ())]  -> Window a -> IO (Frame ())
frameTool props parent
  = frameEx (frameDefaultStyle .-. wxFRAME_TOOL_WINDOW .-. wxFRAME_FLOAT_ON_PARENT)  props parent


-- | Create a top-level frame window in a custom style.
frameEx :: Style -> [Prop (Frame ())]  -> Window a -> IO (Frame ())
frameEx style props parent
  = do f <- frameCreate parent idAny "" rectNull (style .+. wxTAB_TRAVERSAL)
       wxcAppSetTopWindow f
       set f [bgcolor := white, visible := True,clientSize := sizeZero]
       set f props
       return f

-- TODO: generalize this into a class.
-- | The image of a frame. 
image :: WriteAttr (Frame a) FilePath
image
  = writeAttr "image"  frameSetIconFromFile

{-
instance Labeled (Frame a) where
  label
    = newAttr "frame-label" frameGetTitle frameSetTitle
-}

instance Form (Frame a) where
  layout
    = writeAttr "layout" windowSetLayout

instance Closeable (Frame a) where
  close f
    = unitIO (windowClose f True {- force? -})