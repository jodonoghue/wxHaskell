{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Dialogs
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Defines common dialogs.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Dialogs
    ( -- * Generic 
      Dialog
     -- * Messages
    , errorDialog, warningDialog, infoDialog
    , confirmDialog, proceedDialog
      -- * Files
    , fileOpenDialog, filesOpenDialog
    , fileSaveDialog
    , dirOpenDialog
      -- * Misc.
    , fontDialog
    , colorDialog
    , passwordDialog
    , textDialog
    , numberDialog
    ) where

import Graphics.UI.WXH.WxcClasses
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.Dialogs

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes

instance Form (Dialog a) where
  layout
    = writeAttr "layout" windowSetLayout
