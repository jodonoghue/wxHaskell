{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Frame
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Frames.
    
 * Instances: 'HasImage', 'Form', 'Closeable', 'Framed' -- 
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
    -- * Internal
    , resizeableFlags, maximizeableFlags, minimizeableFlags, closeableFlags
    ) where

import Graphics.UI.WXCore

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Window
import Graphics.UI.WX.Events

defaultStyle 
  = frameDefaultStyle .+. wxTAB_TRAVERSAL -- .+. wxNO_FULL_REPAINT_ON_RESIZE

-- | Create a top-level frame window.
frame :: [Prop (Frame ())]  -> IO (Frame ())
frame props
  = frameEx defaultStyle props objectNull

-- | Create a top-level frame window that is not resizeable.
frameFixed :: [Prop (Frame ())]  -> IO (Frame ())
frameFixed props
  = frameEx (defaultStyle .-. wxMAXIMIZE_BOX .-. wxRESIZE_BORDER)  props objectNull

-- | Create a tool window; floats on the parent and has a small caption.
frameTool :: [Prop (Frame ())]  -> Window a -> IO (Frame ())
frameTool props parent
  = frameEx (defaultStyle .-. wxFRAME_TOOL_WINDOW .-. wxFRAME_FLOAT_ON_PARENT)  props parent


-- | Create a top-level frame window in a custom style.
frameEx :: Style -> [Prop (Frame ())]  -> Window a -> IO (Frame ())
frameEx style props parent
  = do f <- frameCreate parent idAny "" rectNull 
              ( minimizeableFlags props 
              $ maximizeableFlags props 
              $ clipChildrenFlags props 
              $ resizeableFlags props   
              $ closeableFlags props
              $ fullRepaintOnResizeFlags props style)
       wxcAppSetTopWindow f
       let initProps = (if (containsProp "visible" props)
                         then [] else [visible := True]) ++
                       (if (containsProp "clientSize" props)
                         then [] else [clientSize := sizeZero]) ++
                       [bgcolor := colorSystem Color3DFace]
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


instance Framed (Frame a) where
  resizeable
    = windowResizeable

  maximizeable
    = windowMaximizeable

  minimizeable
    = windowMinimizeable

  closeable
    = windowCloseable


instance Framed (Dialog a) where
  resizeable
    = windowResizeable

  maximizeable
    = windowMaximizeable

  minimizeable
    = windowMinimizeable

  closeable
    = windowCloseable
  


{--------------------------------------------------------------------------
  MDI frames
--------------------------------------------------------------------------}
-- | Create an MDI parent frame.
mdiParentFrame :: [Prop (MDIParentFrame ())] -> IO (MDIParentFrame ())
mdiParentFrame props
  = mdiParentFrameEx objectNull defaultStyle props


-- | Create an MDI parent frame with a custom style.
mdiParentFrameEx :: Window a -> Style -> [Prop (MDIParentFrame ())] -> IO (MDIParentFrame ())
mdiParentFrameEx parent stl props
  = do f <- mdiParentFrameCreate parent idAny "" rectNull 
              ( minimizeableFlags props 
              $ maximizeableFlags props 
              $ clipChildrenFlags props 
              $ resizeableFlags props
              $ closeableFlags props
              $ fullRepaintOnResizeFlags props stl)
       wxcAppSetTopWindow f
       set f [visible := True, clientSize := sizeZero]
       set f props
       return f

-- | Create a MDI child frame.
mdiChildFrame :: MDIParentFrame a -> [Prop (MDIChildFrame ())] -> IO (MDIChildFrame ())
mdiChildFrame parent props
  = mdiChildFrameEx parent defaultStyle props

-- | Create a MDI child frame with a custom style.
mdiChildFrameEx :: MDIParentFrame a -> Style -> [Prop (MDIChildFrame ())] -> IO (MDIChildFrame ())
mdiChildFrameEx parent stl props
  = do f <- mdiChildFrameCreate parent idAny "" rectNull 
              ( minimizeableFlags props 
              $ maximizeableFlags props 
              $ clipChildrenFlags props 
              $ resizeableFlags props   
              $ fullRepaintOnResizeFlags props stl)
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


{--------------------------------------------------------------------------
  Framed instances
--------------------------------------------------------------------------}
-- | Display a resize border on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowResizeable :: Attr (Window a) Bool
windowResizeable
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
  = case getPropValue windowResizeable props of
      Just True  -> stl .+. wxRESIZE_BORDER
      Just False -> stl .-. wxRESIZE_BORDER
      Nothing    -> stl


-- | Display a maximize box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowMaximizeable :: Attr (Window a) Bool
windowMaximizeable
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
  = case getPropValue windowMaximizeable props of
      Just True  -> stl .+. wxMAXIMIZE_BOX
      Just False -> stl .-. wxMAXIMIZE_BOX
      Nothing    -> stl


-- | Display a minimize box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowMinimizeable :: Attr (Window a) Bool
windowMinimizeable
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
  = case getPropValue windowMinimizeable props of
      Just True  -> stl .+. wxMINIMIZE_BOX
      Just False -> stl .-. wxMINIMIZE_BOX
      Nothing    -> stl


-- | Display a close box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowCloseable :: Attr (Window a) Bool
windowCloseable
  = reflectiveAttr "closeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxCLOSE_BOX s)
    setFlag w min
      = set w [style :~ \stl -> if min then stl .+. wxCLOSE_BOX else stl .-. wxCLOSE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'closeable' flag out of the properties
closeableFlags :: [Prop (Window a)] -> Int -> Int
closeableFlags props stl
  = case getPropValue windowCloseable props of
      Just True  -> stl .+. wxCLOSE_BOX
      Just False -> stl .-. wxCLOSE_BOX
      Nothing    -> stl

