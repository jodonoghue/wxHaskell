--------------------------------------------------------------------------------
{-| Module      :  Controls
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable
-}
--------------------------------------------------------------------------------
module Graphics.UI.WXCore.Controls
    ( 
      -- * Log
      textCtrlMakeLogActiveTarget
    , logDeleteAndSetActiveTarget
      -- * Wrappers
    , listBoxGetSelectionList
    ) where

import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types

import Foreign.Marshal.Array

-- | Return the current selection in a listbox.
listBoxGetSelectionList :: ListBox a -> IO [Int]
listBoxGetSelectionList listBox
  = do n <- listBoxGetSelections listBox ptrNull 0
       let count = abs n
       allocaArray count $ \carr ->
        do listBoxGetSelections listBox carr count
           xs <- peekArray count carr
           return (map fromCInt xs)

-- | Sets the active log target and deletes the old one.
logDeleteAndSetActiveTarget :: Log a -> IO ()
logDeleteAndSetActiveTarget log
  = do oldlog <- logSetActiveTarget log
       logDelete oldlog
       

-- | Set a text control as a log target.
textCtrlMakeLogActiveTarget :: TextCtrl a -> IO ()
textCtrlMakeLogActiveTarget textCtrl
  = do log <- logTextCtrlCreate textCtrl
       logDeleteAndSetActiveTarget log
       