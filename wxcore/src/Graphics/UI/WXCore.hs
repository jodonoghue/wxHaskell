{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------------------
{-| Module      :  WXCore
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    The "WXCore" module is the interface to the core wxWindows functionality.
    
    The library contains the automatically generated interface to the raw
    wxWindows API in "Graphics.UI.WXCore.WxcClasses", "Graphics.UI.WXCore.WxcClassTypes",
    and "Graphics.UI.WXCore.WxcDefs". 

    The other helper modules contain convenient wrappers but only use functional
    abstractions: no type classes or other fancy Haskell features. (The
    higher-level "Graphics.UI.WX" module provides such abstractions.)
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore
        (
        -- * Re-exports
          module Graphics.UI.WXCore.WxcDefs
        , module Graphics.UI.WXCore.WxcClasses
        , module Graphics.UI.WXCore.WxcClassTypes
        , module Graphics.UI.WXCore.Types
        , module Graphics.UI.WXCore.Process
        , module Graphics.UI.WXCore.Draw
        , module Graphics.UI.WXCore.Events
        , module Graphics.UI.WXCore.Frame
        , module Graphics.UI.WXCore.Dialogs
        , module Graphics.UI.WXCore.Controls
        , module Graphics.UI.WXCore.Layout
        , module Graphics.UI.WXCore.Image

        -- * Run
        , run
        ) where

import System.Mem( performGC )

import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcClassTypes

import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Process
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.Draw
import Graphics.UI.WXCore.Frame
import Graphics.UI.WXCore.Dialogs
import Graphics.UI.WXCore.Controls
import Graphics.UI.WXCore.Layout
import Graphics.UI.WXCore.Image

-- | Start the event loop. Takes an initialisation action as argument.
-- Except for 'run', the functions in the WXH library can only be called
-- from this intialisation action or from event handlers, or otherwise bad
-- things will happen :-)
run :: IO a -> IO ()
run init
  = do appOnInit (do wxcAppInitAllImageHandlers
                     init
                     return ())
       performGC