-----------------------------------------------------------------------------------------
{-| Module      :  Defines
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Exports standard /defines/ of wxWindows.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Defines(
            WxToolkit(..), wxToolkit
            ) where

import Graphics.UI.WXCore.WxcClasses
import System.IO.Unsafe( unsafePerformIO )

{--------------------------------------------------------------------------------
  
--------------------------------------------------------------------------------}

-- | wxWindows library kinds.
data WxToolkit  = WxGTK         -- ^ GTK
                | WxMac         -- ^ MacOS
                | WxMSW         -- ^ Any windows
                | WxMotif       
                | WxMGL         -- ^ SciTech soft MGL
                | WxUniversal
                | WxOSTwo       -- ^ OS\/2
                | WxXEleven     -- ^ X11
                | WxUnknown
                deriving (Eq,Show,Enum)

{-# NOINLINE wxToolkit #-}
-- | Get the current wxWindows library kind.
wxToolkit :: WxToolkit
wxToolkit
  = unsafePerformIO $ findDefine WxUnknown toolkits
  where
    toolkits  = [("__WXGTK__", WxGTK)
                ,("__WXMAC__", WxMac)
                ,("__WXMSW__", WxMSW)
                ,("__WXMOTIF__", WxMotif)
                ,("__WXMGL__", WxMGL)
                ,("__WXUNIVERSAL__", WxUniversal)
                ,("__WXOS2__", WxOSTwo)
                ,("__WXX11__", WxXEleven)
                ]
   
findDefine :: a -> [(String,a)] -> IO a
findDefine def []
  = return def
findDefine def ((name,val):xs)
  = do defined <- isDefined name
       if (defined)
        then return val
        else findDefine def xs