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
    ( 
    -- * Classes
      Drawn, penStyle, pen, penwidth, pencap, penjoin
    , Brushed, brushStyle, brush
      -- * Types
    , DC
    -- ** Brushes
    , BrushStyle(..), BrushKind(..), brushDefault
    -- ** Pens
    , PenKind(..), penDefault, penColored, penTransparent
    -- ** Draw styles
    , HatchStyle(..), CapStyle(..), JoinStyle(..), DashStyle(..)
    -- ** Font
    -- , FontInfo(..)
    , FontFamily(..), FontStyle(..), FontWeight(..)
    , fontNormal, fontSwiss, fontSmall, fontItalic
    -- * Drawing
    , circle, arc, ellipse, ellipticArc
    , line, polyline, polygon
    , drawPoint, drawRect, roundedRect
    , drawText, rotatedText, drawBitmap
    -- * Internal
    , dcWithProps
    ) where

-- for haddock, we import wxh module selectively
-- import Graphics.UI.WXH
import Graphics.UI.WXH.WxcClasses
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.Events
import Graphics.UI.WXH.Draw


import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Window



{--------------------------------------------------------------------------------

--------------------------------------------------------------------------------}
class Drawn w where
  penStyle  :: Attr w PenStyle
  pen       :: Attr w PenKind  
  penwidth  :: Attr w Int
  pencap    :: Attr w CapStyle
  penjoin   :: Attr w JoinStyle

class Brushed w where
  brushStyle :: Attr w BrushStyle
  brush      :: Attr w BrushKind

instance Drawn (DC a) where
  penStyle 
    = newAttr "penStyle" dcGetPenStyle dcSetPenStyle
  pen
      = mapAttr penKind (\x pstyle -> pstyle{ penKind = x }) penStyle

  penwidth
    = mapAttr penWidth (\x pstyle -> pstyle{ penWidth = x }) penStyle

  pencap
    = mapAttr penCap (\x pstyle -> pstyle{ penCap = x }) penStyle
  
  penjoin
    = mapAttr penJoin (\x pstyle -> pstyle{ penJoin = x }) penStyle


instance Brushed (DC a) where
  brushStyle
    = newAttr "brushStyle" dcGetBrushStyle dcSetBrushStyle

  brush
    = mapAttr brushKind (\x bstyle -> bstyle{ brushKind = x }) brushStyle


instance Literate (DC a) where
  font
    = newAttr "font" dcGetFontInfo dcSetFontInfo

instance Colored (DC a) where
  color
    = mapAttr penColor (\color pstyle -> pstyle{ penColor = color }) penStyle

  bgcolor
    = mapAttr brushColor (\color bstyle -> bstyle{ brushColor = color }) brushStyle


-- Save pen/font/brush efficiently.
dcWithProps :: DC a -> [Prop (DC a)] -> IO b -> IO b
dcWithProps dc props io
  | null props = io
  | otherwise  = dcRestoreDrawState dc (do set dc props; io)

-- | Draw a circle given a center point and radius.
circle :: DC a -> Point -> Int -> [Prop (DC a)] -> IO ()
circle dc center radius props
  = dcWithProps dc props (dcDrawCircle dc center radius)

-- | Draw an arc of a circle. Takes the center of the circle, 
-- its radius and a starting and ending point relative to the
-- three-o\'clock position. Angles are in degrees and positive
-- values denote a counter clockwise motion. If the angles are
-- equal, an entire circle is drawn.
arc :: DC a -> Point -> Int -> Double -> Double -> [Prop (DC a)] -> IO ()
arc dc center radius start end props
  = ellipticArc dc bounds start end props
  where
    bounds 
      = rect (pt (px center - radius) (py center - radius)) (sz (2*radius) (2*radius))
{-
  = dcWithProps dc props (dcDrawArc dc center (point start) (point end) )
  where
    point angle
      = let radians = (2*pi*angle)/360
            x       = px center + round (cos radians * fromIntegral radius)
            y       = py center - round (sin radians * fromIntegral radius)
        in (pt x y)
-}

-- | Draw an ellipse, bounded by a certain rectangle.
ellipse :: DC a -> Rect -> [Prop (DC a)] -> IO ()
ellipse dc rect props
  = dcWithProps dc props (dcDrawEllipse dc rect)

-- | Draw an elliptic arc. Takes the bounding rectangle, 
-- and a starting and ending point relative to the
-- three-o\'clock position from the center of the rectangle. 
-- Angles are in degrees and positive
-- values denote a counter clockwise motion. If the angles are
-- equal, an entire ellipse is drawn.
ellipticArc :: DC a -> Rect -> Double -> Double -> [Prop (DC a)] -> IO ()
ellipticArc dc rect start end props
  = dcWithProps dc props (dcDrawEllipticArc dc rect start end)

-- | Draw a line.
line :: DC a -> Point -> Point -> [Prop (DC a)] -> IO ()
line dc start end props
  = dcWithProps dc props (dcDrawLine dc start end)

-- | Draw a polyline.
polyline :: DC a -> [Point] -> [Prop (DC a)] -> IO ()
polyline dc points props
  = dcWithProps dc props (drawLines dc points)

-- | Draw a polygon. The polygon is filled with the odd-even rule.
-- Note that the polygon is automatically closed.
polygon :: DC a -> [Point] -> [Prop (DC a)] -> IO ()
polygon dc points props
  = dcWithProps dc props (drawPolygon dc points)


-- | Draw a single point. 
drawPoint :: DC a -> Point -> [Prop (DC a)] -> IO ()
drawPoint dc center props
  = dcWithProps dc props (dcDrawPoint dc center)

-- | Draw a rectangle.
drawRect :: DC a -> Rect -> [Prop (DC a)] -> IO ()
drawRect dc rect props
  = dcWithProps dc props (dcDrawRectangle dc rect)

-- | Draw a rectangle with rounded corners. The corners are
-- quarter circles with the given radius.
-- If radius is positive, the value is assumed to be the radius of the rounded corner. 
-- If radius is negative, the absolute value is assumed to be the proportion of the smallest 
-- dimension of the rectangle. This means that the corner can be a sensible size relative to 
-- the size of the rectangle, and also avoids the strange effects X produces when the corners 
-- are too big for the rectangle.
roundedRect :: DC a -> Rect -> Double -> [Prop (DC a)] -> IO ()
roundedRect dc rect radius props
  = dcWithProps dc props (dcDrawRoundedRectangle dc rect radius)

-- | Draw text.
drawText :: DC a -> String -> Point -> [Prop (DC a)] -> IO ()
drawText dc text point props
  = dcWithProps dc props (dcDrawText dc text point)

-- | Draw rotated text. Takes an angle in degrees relative to the
-- three-o\'clock position.
rotatedText :: DC a -> String -> Point -> Double -> [Prop (DC a)] -> IO ()
rotatedText dc text point angle props
  = dcWithProps dc props (dcDrawRotatedText dc text point angle)

-- | Draw a bitmap. Takes a bitmap, a point and a boolean
-- that is 'True' when the bitmap is drawn with a transparency mask.
drawBitmap :: DC a -> Bitmap () -> Point -> Bool -> [Prop (DC a)] -> IO ()
drawBitmap dc bitmap point transparent props
  = if bitmap == nullBitmap || objectIsNull bitmap 
     then return ()
     else do ok <- bitmapOk bitmap
             if not ok 
              then return ()
              else dcWithProps dc props (dcDrawBitmap dc bitmap point transparent)