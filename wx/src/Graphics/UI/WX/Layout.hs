{-# OPTIONS -fglasgow-exts  #-}
-----------------------------------------------------------------------------------------
{-| Module      :  Layout
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable


-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WX.Layout
             (
             -- * Classes
               Form, layout
             -- * Layout
             , module Graphics.UI.WXH.Layout
             ) where

import Graphics.UI.WXH.Layout
import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes

{--------------------------------------------------------------------

--------------------------------------------------------------------}
-- | Forms can contain other widgets.
class Form w where
  -- | Set the layout of the child widgets.
  layout :: Attr w Layout