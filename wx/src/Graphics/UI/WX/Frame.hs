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
    , initialFrame, initialResizeable, initialMaximizeable, initialMinimizeable, initialCloseable
    ) where

import Graphics.UI.WXCore

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Window
import Graphics.UI.WX.Events

defaultStyle 
  = frameDefaultStyle -- .+. wxTAB_TRAVERSAL -- .+. wxNO_FULL_REPAINT_ON_RESIZE

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
  = feed2 props style $
    initialFrame $ \id rect txt -> \props style ->
    do f <- frameCreate parent id txt rect style
       let initProps = (if (containsProperty visible props)
                        then [] else [visible := True]) ++
                       (if (containsProperty bgcolor props)
                        then [] else [bgcolor := colorSystem Color3DFace])
       set f initProps
       set f props
       return f
     

-- | initial Frame flags
initialFrame :: (Id -> Rect -> String -> [Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialFrame cont 
  = initialContainer    $ \id rect ->
    initialText         $ \txt ->
    initialResizeable   $
    initialMinimizeable $
    initialMaximizeable $
    initialCloseable    $
    initialClipChildren $
    initialFullRepaintOnResize $ 
    cont id rect txt 
             

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
  = feed2 props stl $
    initialFrame $ \id rect txt -> \props stl ->
    do f <- mdiParentFrameCreate parent id txt rect stl
       let initProps = (if (containsProperty visible props)
                        then [] else [visible := True]) ++
                       (if (containsProperty bgcolor props)
                        then [] else [bgcolor := colorSystem Color3DFace])
       set f initProps
       set f props
       return f


-- | Create a MDI child frame.
mdiChildFrame :: MDIParentFrame a -> [Prop (MDIChildFrame ())] -> IO (MDIChildFrame ())
mdiChildFrame parent props
  = mdiChildFrameEx parent defaultStyle props

-- | Create a MDI child frame with a custom style.
mdiChildFrameEx :: MDIParentFrame a -> Style -> [Prop (MDIChildFrame ())] -> IO (MDIChildFrame ())
mdiChildFrameEx parent stl props
  = feed2 props stl $
    initialFrame $ \id rect txt -> \props stl ->
    do f <- mdiChildFrameCreate parent id txt rect stl
       let initProps = (if (containsProperty visible props)
                         then [] else [visible := True]) ++
                       (if (containsProperty bgcolor props)
                         then [] else [bgcolor := colorSystem Color3DFace])
       set f initProps
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
-- | Display a resize border on a 'Frame' or 'Dialog' window.  Also enables or
-- disables the the maximize box.
-- This attribute must be set at creation time.
windowResizeable :: CreateAttr (Window a) Bool
windowResizeable
  = reflectiveAttr "resizeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxRESIZE_BORDER s)
    setFlag w resize
      = set w [style :~ \stl -> if resize 
                                 then stl .+. wxRESIZE_BORDER .+. wxMAXIMIZE_BOX
                                 else stl .-. wxRESIZE_BORDER .-. wxMAXIMIZE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'windowResizable' flag in of the properties
initialResizeable :: ([Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialResizeable 
  = withStyleProperty windowResizeable (wxRESIZE_BORDER .+. wxMAXIMIZE_BOX) 


-- | Display a maximize box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowMaximizeable :: CreateAttr (Window a) Bool
windowMaximizeable
  = reflectiveAttr "maximizeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxMAXIMIZE_BOX s)
    setFlag w max
      = set w [style :~ \stl -> if max then stl .+. wxMAXIMIZE_BOX else stl .-. wxMAXIMIZE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'windowMaximizable' flag in of the properties
initialMaximizeable :: ([Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialMaximizeable 
  = withStyleProperty windowMaximizeable wxMAXIMIZE_BOX 


-- | Display a minimize box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowMinimizeable :: CreateAttr (Window a) Bool
windowMinimizeable
  = reflectiveAttr "minimizeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxMINIMIZE_BOX s)
    setFlag w min
      = set w [style :~ \stl -> if min then stl .+. wxMINIMIZE_BOX else stl .-. wxMINIMIZE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'windowMinimizable' flag in of the properties
initialMinimizeable :: ([Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialMinimizeable 
  = withStyleProperty windowMinimizeable wxMINIMIZE_BOX 


-- | Display a close box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowCloseable :: CreateAttr (Window a) Bool
windowCloseable
  = reflectiveAttr "closeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxCLOSE_BOX s)
    setFlag w min
      = set w [style :~ \stl -> if min then stl .+. wxCLOSE_BOX else stl .-. wxCLOSE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'windowMinimizable' flag in of the properties
initialCloseable :: ([Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialCloseable 
  = withStyleProperty windowCloseable wxCLOSE_BOX 
