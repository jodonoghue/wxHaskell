--------------------------------------------------------------------------------
{-| Module      :  Controls
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable
-}
--------------------------------------------------------------------------------
module Graphics.UI.WXH.Controls
    ( -- * Wrappers
      listBoxGetSelectionList
    ) where

import Graphics.UI.WXH.WxcTypes
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.WxcClasses
import Graphics.UI.WXH.Types

import Foreign.Marshal.Array

listBoxGetSelectionList :: ListBox a -> IO [Int]
listBoxGetSelectionList listBox
  = do n <- listBoxGetSelections listBox ptrNull 0
       let count = abs n
       allocaArray count $ \carr ->
        do listBoxGetSelections listBox carr count
           xs <- peekArray count carr
           return (map fromCInt xs)