{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Frame
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Frames.
    
 * Instances: 'HasImage', 'Form', 'Closable' -- 
             'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
             

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Frame
    ( -- * Frames
      Frame, frame, frameFixed, frameTool, frameEx
      -- * MDI Frames
    , MDIParentFrame, MDIChildFrame
    , mdiParentFrame, mdiChildFrame
    , mdiParentFrameEx, mdiChildFrameEx
     -- ** Operations
    , activeChild, activateNext, activatePrevious, arrangeIcons
    , cascade, tile
    ) where

import Graphics.UI.WXCore

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Window

-- | Create a top-level frame window.
frame :: [Prop (Frame ())]  -> IO (Frame ())
frame props
  = frameEx (frameDefaultStyle .+. wxTAB_TRAVERSAL) props objectNull

-- | Create a top-level frame window that is not resizeable.
frameFixed :: [Prop (Frame ())]  -> IO (Frame ())
frameFixed props
  = frameEx (frameDefaultStyle .+. wxTAB_TRAVERSAL .-. wxMAXIMIZE_BOX .-. wxRESIZE_BORDER)  props objectNull

-- | Create a tool window; floats on the parent and has a small caption.
frameTool :: [Prop (Frame ())]  -> Window a -> IO (Frame ())
frameTool props parent
  = frameEx (frameDefaultStyle .+. wxTAB_TRAVERSAL .-. wxFRAME_TOOL_WINDOW .-. wxFRAME_FLOAT_ON_PARENT)  props parent


-- | Create a top-level frame window in a custom style.
frameEx :: Style -> [Prop (Frame ())]  -> Window a -> IO (Frame ())
frameEx style props parent
  = do f <- frameCreate parent idAny "" rectNull (style .+. noFullRepaintOnResize props)
       wxcAppSetTopWindow f
       let initProps = if (containsProp "visible" props)
                        then [] else [visible := True] ++
                       if (containsProp "clientSize" props)
                        then [] else [clientSize := sizeZero]
       set f initProps
       set f props
       return f


-- The image of a frame. 
instance HasImage (Frame a) where
  image
    = writeAttr "image"  frameSetIconFromFile


instance Form (Frame a) where
  layout
    = writeAttr "layout" windowSetLayout

instance Closeable (Frame a) where
  close f
    = unitIO (windowClose f True {- force? -})

{--------------------------------------------------------------------------
  MDI frames
--------------------------------------------------------------------------}
-- | Create an MDI parent frame.
mdiParentFrame :: [Prop (MDIParentFrame ())] -> IO (MDIParentFrame ())
mdiParentFrame props
  = mdiParentFrameEx objectNull (frameDefaultStyle .+. wxTAB_TRAVERSAL) props


-- | Create an MDI parent frame with a custom style.
mdiParentFrameEx :: Window a -> Style -> [Prop (MDIParentFrame ())] -> IO (MDIParentFrame ())
mdiParentFrameEx parent stl props
  = do f <- mdiParentFrameCreate parent idAny "" rectNull (stl .+. noFullRepaintOnResize props)
       wxcAppSetTopWindow f
       set f [visible := True, clientSize := sizeZero]
       set f props
       return f

-- | Create a MDI child frame.
mdiChildFrame :: MDIParentFrame a -> [Prop (MDIChildFrame ())] -> IO (MDIChildFrame ())
mdiChildFrame parent props
  = mdiChildFrameEx parent (frameDefaultStyle .+. wxTAB_TRAVERSAL) props

-- | Create a MDI child frame with a custom style.
mdiChildFrameEx :: MDIParentFrame a -> Style -> [Prop (MDIChildFrame ())] -> IO (MDIChildFrame ())
mdiChildFrameEx parent stl props
  = do f <- mdiChildFrameCreate parent idAny "" rectNull (stl .+. noFullRepaintOnResize props)
       set f [visible := True, clientSize := sizeZero]
       set f props
       return f

-- | Return the active child frame ('objectIsNull' when no child is active)
activeChild :: ReadAttr (MDIParentFrame a) (MDIChildFrame ())
activeChild = readAttr "activeChild" mdiParentFrameGetActiveChild

-- | Activate the next child frame.
activateNext :: MDIParentFrame a -> IO ()
activateNext  = mdiParentFrameActivateNext

-- | Activate the previous child frame
activatePrevious :: MDIParentFrame a -> IO ()
activatePrevious  = mdiParentFrameActivatePrevious

-- | Arrange iconized mdi child frames.
arrangeIcons :: MDIParentFrame a -> IO ()
arrangeIcons = mdiParentFrameArrangeIcons

-- | Cascade the child frames.
cascade :: MDIParentFrame a -> IO ()
cascade = mdiParentFrameCascade 

-- | Tile the child frames
tile :: MDIParentFrame a -> IO ()
tile = mdiParentFrameTile
