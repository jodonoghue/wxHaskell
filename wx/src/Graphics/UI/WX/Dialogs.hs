--------------------------------------------------------------------------------
{-| Module      :  Dialogs
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Dialogs
    ( -- * Messages
      errorDialog, warningDialog, infoDialog
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

import Graphics.UI.WXH.Dialogs