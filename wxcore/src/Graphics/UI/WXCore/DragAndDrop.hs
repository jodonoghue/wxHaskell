{-# OPTIONS -fglasgow-exts #-}
{-----------------------------------------------------------------------------------------
    Module      :  DragAndDrop
    Copyright   :  (c) shelarcy 2007
    License     :  wxWidgets

    Maintainer  :  shelarcy@gmail.com
    Stability   :  provisional
    Portability :  portable

    Drag & Drop events.
-----------------------------------------------------------------------------------------}
module Graphics.UI.WXCore.DragAndDrop (
        -- * Drop Targets
          dropTarget
        -- * Create Drop Soruces
        , dropSource
        , dropSourceWithCursor
        , dropSourceWithCursorByString
        , dropSourceWithIcon

        -- * Set event handlers
        -- ** Drop Target events
        , DragResult (..)
        , dropTargetOnData
        , dropTargetOnDrop
        , dropTargetOnEnter
        , dropTargetOnDragOver
        , dropTargetOnLeave
        
        -- ** On DragAndDropEvent
        , DragMode (..)
        , dragAndDrop
        
        -- ** Special handler for Drop File event
        , fileDropTarget
        -- ** Special handler for Drop Text event 
        , textDropTarget
        ) where

import Graphics.UI.WXCore.Defines
import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcObject
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcClassTypes
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassesMZ

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils


{--------------------------------------------------------------------------------
  Drop Target
--------------------------------------------------------------------------------}
-- | Set a drop target window and 'DataObject' that is associated with drop event.
dropTarget :: Window a -> DataObject b -> IO (WXCDropTarget  ())
dropTarget window wxdata = do
    drop <- wxcDropTargetCreate nullPtr
    dropTargetSetDataObject drop wxdata
    windowSetDropTarget window drop
    return drop

{-----------------------------------------------------------------------------------------
  Drag and Drop events
-----------------------------------------------------------------------------------------}
-- | Drag results
data DragResult
    = DragError
    | DragNone
    | DragCopy
    | DragMove
    | DragLink
    | DragCancel
    | DragUnknown
   deriving (Eq,Show)

dragResults :: [(Int, DragResult)]
dragResults
    = [(wxDRAG_ERROR   ,DragError)
      ,(wxDRAG_NONE    ,DragNone)
      ,(wxDRAG_COPY    ,DragCopy)
      ,(wxDRAG_MOVE    ,DragMove)
      ,(wxDRAG_LINK    ,DragLink)
      ,(wxDRAG_CANCEL  ,DragCancel)]

fromDragResult :: DragResult -> Int
fromDragResult drag
  = case drag of
      DragError   -> wxDRAG_ERROR
      DragNone    -> wxDRAG_NONE
      DragCopy    -> wxDRAG_COPY
      DragMove    -> wxDRAG_MOVE
      DragLink    -> wxDRAG_LINK
      DragCancel  -> wxDRAG_CANCEL
      DragUnknown -> wxDRAG_ERROR

toDragResult :: Int -> DragResult
toDragResult drag
 = case lookup drag dragResults of
      Just x -> x
      Nothing -> DragError

-- | Set an event handler that is called when the drop target can be filled with data.
-- This function require to use 'dropTargetGetData' in your event handler to fill data.
dropTargetOnData :: DropTarget a -> (Point -> DragResult -> IO DragResult) -> IO ()
dropTargetOnData drop event = do
    funPtr <- dragThreeFuncHandler event
    wxcDropTargetSetOnData (objectCast drop) (toCFunPtr funPtr)

-- | Set an event handler for an drop command in a drop target.
dropTargetOnDrop :: DropTarget a -> (Point -> IO Bool) -> IO ()
dropTargetOnDrop drop event = do
    funPtr <- dragTwoFuncHandler event
    wxcDropTargetSetOnDrop (objectCast drop) (toCFunPtr funPtr)

-- | Set an event handler for an enter command in a drop target.
dropTargetOnEnter :: DropTarget a -> (Point -> DragResult -> IO DragResult) -> IO ()
dropTargetOnEnter drop event = do
    funPtr <- dragThreeFuncHandler event
    wxcDropTargetSetOnEnter (objectCast drop) (toCFunPtr funPtr)

-- | Set an event handler for a drag over command in a drop target.
dropTargetOnDragOver :: DropTarget a -> (Point -> DragResult -> IO DragResult) -> IO ()
dropTargetOnDragOver drop event = do
    funPtr <- dragThreeFuncHandler event
    wxcDropTargetSetOnDragOver (objectCast drop) (toCFunPtr funPtr)

-- | Set an event handler for a leave command in a drop target.
dropTargetOnLeave :: DropTarget a -> (IO ()) -> IO ()
dropTargetOnLeave drop event = do
    funPtr <- dragZeroFuncHandler event
    wxcDropTargetSetOnLeave (objectCast drop) (toCFunPtr funPtr)

dragZeroFuncHandler event =
    dragZeroFunc $ \obj -> do
    event

dragTwoFuncHandler event =
    dragTwoFunc $ \obj x y -> do
    result <- event (point (fromIntegral x) (fromIntegral y))
    return $ fromBool result

dragThreeFuncHandler event =
    dragThreeFunc $ \obj x y pre -> do
    result <- event (point (fromIntegral x) (fromIntegral y)) (toDragResult $ fromIntegral pre)
    return $ fromIntegral $ fromDragResult result

-- | Set an event handler for a drag & drop command between drag source window and drop
-- target. You must set 'dropTarget' before use this action.
-- And If you use 'fileDropTarget' or 'textDropTarget', you need not use this.
dragAndDrop :: DropSource a -> DragMode -> (DragResult -> IO ()) -> IO ()
dragAndDrop drSrc flag event = do
    result <- dropSourceDoDragDrop drSrc (fromDragMode flag)
    case lookup result dragResults of
      Just x -> event x
      Nothing -> return ()

-- | Create 'DropSource'. Then 'dragAndDrop' replace target\'s 'DataObject' by this 'DataObject'.
dropSource :: DataObject a -> Window b -> IO (DropSource ())
dropSource wxdata win =
    withObjectPtr nullIcon $ \icon ->
    withObjectPtr nullIcon $ \icon ->
    withObjectPtr nullIcon $ \icon ->
    dropSourceCreate wxdata win icon icon icon

-- | On Windows or Mac OS X platform, you must choose this function or 'dropSourceWithCursorByString',
-- if you want to use Custom Cursor for Drag & Drop event. 'dropSourceWithIcon' doesn't work these
-- platform, and this function doesn't work other platforms.
dropSourceWithCursor :: DataObject a -> Window b -> Cursor c -> Cursor c -> Cursor c -> IO (DropSource ())
dropSourceWithCursor wxdata win copy move none =
    withObjectPtr copy $ \dndCopy ->
    withObjectPtr move $ \dndMove ->
    withObjectPtr none $ \dndNone ->
    dropSourceCreate wxdata win dndCopy dndMove dndNone

dropSourceWithIcon :: DataObject a -> Window b -> Icon c -> Icon c -> Icon c -> IO (DropSource ())
dropSourceWithIcon wxdata win copy move none =
    withObjectPtr copy $ \dndCopy ->
    withObjectPtr move $ \dndMove ->
    withObjectPtr none $ \dndNone ->
    dropSourceCreate wxdata win dndCopy dndMove dndNone

dropSourceWithCursorByString :: DataObject a -> Window b -> String -> String -> String -> IO (DropSource ())
dropSourceWithCursorByString wxdata win copy move none =
   case wxToolkit of
     WxMSW -> do
         dndCopy <- cursorCreateFromFile copy
         dndMove <- cursorCreateFromFile move
         dndNone <- cursorCreateFromFile none
         dropSourceWithCursor wxdata win dndCopy dndMove dndNone
     WxMac -> do
         dndCopy <- cursorCreateFromFile copy
         dndMove <- cursorCreateFromFile move
         dndNone <- cursorCreateFromFile none
         dropSourceWithCursor wxdata win dndCopy dndMove dndNone
     _ -> do
         dndCopy <- iconCreateFromFile copy sizeNull
         dndMove <- iconCreateFromFile move sizeNull
         dndNone <- iconCreateFromFile none sizeNull
         dropSourceWithIcon wxdata win dndCopy dndMove dndNone


-- | Set an event handler that is called when text is dropped in target window.
textDropTarget :: Window a -> TextDataObject b -> (Point -> String -> IO ()) -> IO ()
textDropTarget window textData event = do
    funPtr <- dropTextHandler event
    textDrop <- wxcTextDropTargetCreate nullPtr (toCFunPtr funPtr)
    dropTargetSetDataObject textDrop textData
    windowSetDropTarget window textDrop

dropTextHandler event =
    wrapTextDropHandler $ \obj x y cstr -> do
    str <- peekCWString cstr
    event (point (fromIntegral x) (fromIntegral y)) str

-- | Set an event handler that is called when files are dropped in target window.
fileDropTarget :: Window a -> (Point -> [String] -> IO ()) -> IO ()
fileDropTarget window event = do
    funPtr <- dropFileHandler event
    fileDrop <- wxcFileDropTargetCreate nullPtr (toCFunPtr funPtr)
    windowSetDropTarget window fileDrop

dropFileHandler event =
    wrapFileDropHandler $ \obj x y carr size -> do
    arr <- peekArray (fromIntegral size) carr
    files <- mapM peekCWString arr
    event (point (fromIntegral x) (fromIntegral y)) files

data DragMode = CopyOnly | AllowMove | Default
              deriving (Eq,Show)
              -- deriving (Eq,Show,Read,Typeable)

fromDragMode :: DragMode -> Int
fromDragMode mode
  = case mode of
      CopyOnly  -> wxDRAG_COPYONLY
      AllowMove -> wxDRAG_ALLOWMOVE
      Default   -> wxDRAG_DEFALUTMOVE

foreign import ccall "wrapper" dragZeroFunc :: (Ptr obj -> IO ()) -> IO (FunPtr (Ptr obj -> IO ()))
foreign import ccall "wrapper" dragTwoFunc :: (Ptr obj -> CInt -> CInt -> IO CInt) -> IO (FunPtr (Ptr obj -> CInt -> CInt -> IO CInt))
foreign import ccall "wrapper" dragThreeFunc :: (Ptr obj -> CInt -> CInt -> CInt -> IO CInt) -> IO (FunPtr (Ptr obj -> CInt -> CInt -> CInt -> IO CInt))
foreign import ccall "wrapper" wrapTextDropHandler :: (Ptr obj -> CInt -> CInt -> Ptr CWchar -> IO ()) -> IO (FunPtr (Ptr obj -> CInt -> CInt -> Ptr CWchar -> IO ()))
foreign import ccall "wrapper" wrapFileDropHandler :: (Ptr obj -> CInt -> CInt -> Ptr (Ptr CWchar) -> CInt -> IO ()) -> IO (FunPtr (Ptr obj -> CInt -> CInt -> Ptr (Ptr CWchar) -> CInt -> IO ()))
