{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Draw
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Draw
    ( DC, withPen
    , Drawn, pen, penwidth, pencap, penjoin
    , Brushed, brush
    , circle
    , BrushKind(..), PenKind(..)
    , HatchStyle(..), CapStyle(..), JoinStyle(..), DashStyle(..)
    ) where

-- for haddock, we import wxh module selectively
-- import Graphics.UI.WXH
import Graphics.UI.WXH.WxcClasses
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.Draw

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Window

{--------------------------------------------------------------------------------

--------------------------------------------------------------------------------}
data Pencil = Pencil{ penStyle   :: Var (Maybe PenStyle)
                    , brushStyle :: Var (Maybe BrushStyle)
                    , fontInfo   :: Var (Maybe FontInfo)
                    }

withPen :: DC a -> [Prop Pencil] -> IO b -> IO b
withPen dc props io
  = withPencil dc (\p -> set p props) io

withPencil :: DC a -> (Pencil -> IO ()) -> IO b -> IO b
withPencil dc update io
  = do vps  <- varCreate Nothing
       vbs  <- varCreate Nothing
       vfi  <- varCreate Nothing
       update (Pencil vps vbs vfi)
       mps  <- varGet vps
       mbs  <- varGet vbs
       mfi  <- varGet vfi
       whenJust mps dcWithPenStyle $
        whenJust mbs dcWithBrushStyle $
         whenJust mfi dcWithFontInfo $
           io

  where
   -- whenJust :: Maybe a -> (DC c -> a -> IO b -> IO b) -> IO b -> IO b
    whenJust mb f io
      = case mb of
          Just x  -> f dc x io
          Nothing -> io

getPenStyle :: Pencil -> (PenStyle -> a) -> IO a
getPenStyle pencil f
  = do x <- varGetMaybe (penStyle pencil) penDefault
       return (f x)

updatePenStyle :: Pencil -> (PenStyle -> PenStyle) -> IO ()
updatePenStyle pencil f
  = varUpdateMaybe (penStyle pencil) penDefault f

getBrushStyle :: Pencil -> (BrushStyle -> a) -> IO a
getBrushStyle pencil f
  = do x <- varGetMaybe (brushStyle pencil) brushDefault
       return (f x)

updateBrushStyle :: Pencil -> (BrushStyle -> BrushStyle) -> IO ()
updateBrushStyle pencil f
  = varUpdateMaybe (brushStyle pencil) brushDefault f


varGetMaybe var def
  = do mb <- varGet var
       case mb of
         Just x  -> return x
         Nothing -> return def

varUpdateMaybe var def f
  = do varUpdate var (\mb -> case mb of
                               Just x  -> Just (f x)
                               Nothing -> Just (f def))
       return ()

class Brushed w where
  brush :: Attr w BrushKind

class Drawn w where
  pen       :: Attr w PenKind
  penwidth  :: Attr w Int
  pencap    :: Attr w CapStyle
  penjoin   :: Attr w JoinStyle

{--------------------------------------------------------------------------------
  Pencil instances
--------------------------------------------------------------------------------}
instance Drawn Pencil where
  pen
    = newAttr "pen pencil" (\pen -> getPenStyle pen penKind) (\pen kind -> updatePenStyle pen (\ps -> ps{ penKind = kind }))

  penwidth
    = newAttr "penwidth pencil" (\pen -> getPenStyle pen penWidth) (\pen width -> updatePenStyle pen (\ps -> ps{ penWidth = width }))

  pencap
    = newAttr "pencap pencil" (\pen -> getPenStyle pen penCap) (\pen cap -> updatePenStyle pen (\ps -> ps{ penCap = cap }))

  penjoin
    = newAttr "penjoin pencil" (\pen -> getPenStyle pen penJoin) (\pen join -> updatePenStyle pen (\ps -> ps{ penJoin = join }))

instance Brushed Pencil where
  brush
    = newAttr "brush pencil" (\pen -> getBrushStyle pen brushKind) (\pen kind -> updateBrushStyle pen (\bs -> bs{ brushKind = kind }))

instance Colored Pencil where
  color
    = newAttr "color pencil"
        (\pen -> getPenStyle pen penColor)
        (\pen color -> updatePenStyle pen (\ps -> ps{ penColor = color }))

  bgcolor
    = newAttr "bgcolor pencil"
        (\pen -> getBrushStyle pen brushColor)
        (\pen color -> updateBrushStyle pen (\bs -> bs{ brushColor = color }))


{--------------------------------------------------------------------------------
  Circle
--------------------------------------------------------------------------------}
newtype Circle = Circle Pencil

unCircle :: Circle -> Pencil
unCircle (Circle pencil)
  = pencil

instance Drawn Circle where
  pen       = mapAttrW unCircle pen
  penwidth  = mapAttrW unCircle penwidth
  pencap    = mapAttrW unCircle pencap
  penjoin   = mapAttrW unCircle penjoin

instance Brushed Circle where
  brush   = mapAttrW unCircle brush

instance Colored Circle where
  color   = mapAttrW unCircle color
  bgcolor = mapAttrW unCircle bgcolor


circle :: DC a -> Point -> Int -> [Prop Circle] -> IO ()
circle dc center radius props
  = withPencil dc (\pen -> set (Circle pen) props) (dcDrawCircle dc center radius)