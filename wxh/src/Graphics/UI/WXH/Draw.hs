{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------------------
{-| Module      :  Draw
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Drawing.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXH.Draw
        (
        -- * DC
          drawLines, drawPolygon, getTextExtent, getFullTextExtent
        -- ** Creation
        , withPaintDC, withClientDC, dcDraw
        -- ** Draw state
        , DrawState, dcEncapsulate, dcGetDrawState, dcSetDrawState, drawStateDelete
        -- ** Double buffering
        , dcBuffer, dcBufferWithRef
        -- * Scrolled windows
        , windowGetViewStart, windowGetViewRect, windowCalcUnscrolledPosition
        -- * Font
        , FontInfo(..), FontFamily(..), FontStyle(..), FontWeight(..)
        , fontDefault, fontSwiss, fontSmall, fontItalic, fontFixed
        , withFontInfo, dcWithFontInfo
        , dcSetFontInfo, dcGetFontInfo
        , fontCreateFromInfo, fontGetFontInfo
        -- * Brush
        , BrushStyle(..), BrushKind(..)
        , HatchStyle(..)
        , brushDefault
        , dcSetBrushStyle, dcGetBrushStyle
        , withBrushStyle, dcWithBrushStyle
        , brushCreateFromStyle, brushGetBrushStyle
        -- * Pen
        , PenStyle(..), PenKind(..), CapStyle(..), JoinStyle(..), DashStyle(..)
        , penDefault, penColored, penTransparent
        , dcSetPenStyle, dcGetPenStyle
        , withPenStyle, dcWithPenStyle
        , penCreateFromStyle, penGetPenStyle
        ) where

import Graphics.UI.WXH.WxcTypes
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.WxcClasses
import Graphics.UI.WXH.Types

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Marshal.Alloc


{--------------------------------------------------------------------------------
  DC creation
--------------------------------------------------------------------------------}
-- | Encloses the computation with 'dcBeginDrawing' and 'dcEndDrawing'.
dcDraw :: DC a -> IO b -> IO b
dcDraw dc io
  = bracket_ (do dcBeginDrawing dc
                 dcSetPenStyle dc penDefault
                 dcSetBrushStyle dc brushDefault) 
             (do dcSetPen dc nullPen
                 dcSetBrush dc nullBrush
                 dcEndDrawing dc) 
             io

-- | Use a 'PaintDC'.
withPaintDC :: Window a -> (PaintDC () -> IO b) -> IO b
withPaintDC window draw
  = bracket (paintDCCreate window) (paintDCDelete) (\dc -> dcDraw dc (draw dc))

-- | Use a 'ClientDC'.
withClientDC :: Window a -> (ClientDC () -> IO b) -> IO b
withClientDC window draw
  = bracket (clientDCCreate window) (clientDCDelete) (\dc -> dcDraw dc (draw dc))


{--------------------------------------------------------------------------------
  Windows
--------------------------------------------------------------------------------}
-- | Get logical view rectangle, adjusted for scrolling.
windowGetViewRect :: Window a -> IO Rect
windowGetViewRect window
  = do size <- windowGetClientSize window
       org  <- windowGetViewStart window
       return (rect org size)

-- | Get logical view start, adjusted for scrolling.
windowGetViewStart :: Window a -> IO Point
windowGetViewStart window
  = do isScrolled <- objectIsScrolledWindow window
       -- adjust coordinates for a scrolled window
       if (isScrolled)
        then do let scrolledWindow = objectCast window
                (Point sx sy) <- scrolledWindowGetViewStart scrolledWindow
                (Point w h)   <- scrolledWindowGetScrollPixelsPerUnit scrolledWindow
                return (Point (w*sx) (h*sy))
        else return pointZero

-- | Get logical coordinates adjusted for scrolling.
windowCalcUnscrolledPosition :: Window a -> Point -> IO Point
windowCalcUnscrolledPosition window p
  = do isScrolled <- objectIsScrolledWindow window
       -- adjust coordinates for a scrolled window
       if (isScrolled)
        then do let scrolledWindow = objectCast window
                scrolledWindowCalcUnscrolledPosition scrolledWindow p
        else return p

{--------------------------------------------------------------------------------
  Font
--------------------------------------------------------------------------------}


-- | Font descriptor. The font is normally specified thru the 'FontFamily', giving
-- some degree of portability. The 'fontFace' can be used to specify the exact font.
data FontInfo
  = FontInfo { fontSize      :: !Int
             , fontFamily    :: !FontFamily
             , fontStyle     :: !FontStyle
             , fontWeight    :: !FontWeight
             , fontUnderline :: !Bool
             , fontFace      :: !String       -- ^ normally @\"\"@
             , fontEncoding  :: !Int          -- ^ normally @wxFONTENCODING_DEFAULT@
             }
  deriving (Eq,Show)

-- | Default 10pt font.
fontDefault :: FontInfo
fontDefault
  = FontInfo 10 FontDefault StyleNormal WeightNormal False "" wxFONTENCODING_DEFAULT

-- | Default 10pt sans-serif font.
fontSwiss :: FontInfo
fontSwiss
  = fontDefault{ fontFamily = FontSwiss }

-- | Default 8pt font.
fontSmall :: FontInfo
fontSmall
  = fontDefault{ fontSize = 8 }

-- | Default 10pt italic.
fontItalic :: FontInfo
fontItalic
  = fontDefault{ fontStyle = StyleItalic }

-- | Monospaced font, 10pt.
fontFixed :: FontInfo
fontFixed 
  = fontDefault{ fontFamily = FontModern }

-- | Standard font families.
data FontFamily
  = FontDefault       -- ^ A system default font.
  | FontDecorative    -- ^ Decorative font.
  | FontRoman         -- ^ Formal serif font.
  | FontScript        -- ^ Hand writing font.
  | FontSwiss         -- ^ Sans-serif font.
  | FontModern        -- ^ Fixed pitch font.
  deriving (Eq,Show)

-- | The font style.
data FontStyle
  = StyleNormal
  | StyleItalic
  | StyleSlant
  deriving (Eq,Show)

-- | The font weight.
data FontWeight
  = WeightNormal
  | WeightBold
  | WeightLight
  deriving (Eq,Show)

-- | Use a font that is automatically deleted at the end of the computation.
withFontInfo :: FontInfo -> (Font () -> IO a) -> IO a
withFontInfo fontInfo f
  = do (font,delete) <- fontCreateFromInfo fontInfo
       finally (f font) delete


-- | Set a font that is automatically deleted at the end of the computation.
dcWithFontInfo :: DC a -> FontInfo -> IO b -> IO b
dcWithFontInfo dc fontInfo io
  = withFontInfo fontInfo $ \font ->
    bracket  (do oldFont <- dcGetFont dc
                 dcSetFont dc font
                 return oldFont)
             (\oldFont ->
              do dcSetFont dc oldFont   -- restore previous font
                 fontDelete oldFont)
             (const io)

-- | Set the font info of a DC.
dcSetFontInfo :: DC a -> FontInfo -> IO ()
dcSetFontInfo dc info
  = do (font,del) <- fontCreateFromInfo info
       finalize del $
        do dcSetFont dc font

-- | Get the current font info.
dcGetFontInfo :: DC a -> IO FontInfo
dcGetFontInfo dc
  = do font <- dcGetFont dc
       finalize (fontDelete font) $
        do fontGetFontInfo font


-- | Create a 'Font' from 'FontInfo'. Returns both the font and a deletion procedure.
fontCreateFromInfo :: FontInfo -> IO (Font (),IO ())
fontCreateFromInfo (FontInfo size family style weight underline face encoding)
  = do font <- fontCreate size cfamily cstyle cweight (intFromBool underline) face encoding
       return (font,when (font /= objectNull) (fontDelete font))
  where
    cfamily
      = case family of
          FontDefault     -> wxDEFAULT
          FontDecorative  -> wxDECORATIVE
          FontRoman       -> wxROMAN
          FontScript      -> wxSCRIPT
          FontSwiss       -> wxSWISS
          FontModern      -> wxMODERN

    cstyle
      = case style of
          StyleNormal     -> wxNORMAL
          StyleItalic     -> wxITALIC
          StyleSlant      -> wxSLANT

    cweight
      = case weight of
          WeightNormal    -> wxNORMAL
          WeightBold      -> wxBOLD
          WeightLight     -> wxLIGHT


-- | Get the 'FontInfo' from a 'Font' object.
fontGetFontInfo :: Font () -> IO FontInfo
fontGetFontInfo font
  = if (objectIsNull font || font == nullFont)
     then return fontDefault
     else do size    <- fontGetPointSize font
             cfamily <- fontGetFamily font
             cstyle  <- fontGetStyle font
             cweight <- fontGetWeight font
             cunderl <- fontGetUnderlined font
             face    <- fontGetFaceName font
             enc     <- fontGetEncoding font
             return (FontInfo size (toFamily cfamily) (toStyle cstyle) (toWeight cweight) (cunderl /= 0) face enc)
   where
    toFamily f
      | f == wxDECORATIVE   = FontDecorative
      | f == wxROMAN        = FontRoman
      | f == wxSCRIPT       = FontScript
      | f == wxSWISS        = FontSwiss
      | f == wxMODERN       = FontModern
      | otherwise           = FontDefault

    toStyle s
      | s == wxITALIC       = StyleItalic
      | s == wxSLANT        = StyleSlant
      | otherwise           = StyleNormal

    toWeight w
      | w == wxBOLD         = WeightBold
      | w == wxLIGHT        = WeightLight
      | otherwise           = WeightNormal


{--------------------------------------------------------------------------------
  Pen
--------------------------------------------------------------------------------}

-- | Pen style.
data PenStyle
  = PenStyle { penKind :: !PenKind, penColor :: !Color, penWidth :: !Int, penCap :: !CapStyle, penJoin :: !JoinStyle }
  deriving (Eq,Show)

-- | Pen kinds.
data PenKind
  = PenTransparent    -- ^ No edge.
  | PenSolid
  | PenDash   { penDash   :: !DashStyle  }
  | PenHatch  { penHatch  :: !HatchStyle }
  | PenStipple{ penBitmap :: !(Bitmap ())}    -- ^ @penColor@ is ignored
  deriving (Eq,Show)

-- | Default pen (@PenStyle PenSolid black 1 CapRound JoinRound@)
penDefault :: PenStyle
penDefault
  = PenStyle PenSolid black 1 CapRound JoinRound

-- | A solid pen with a certain color and width.
penColored :: Color -> Int -> PenStyle
penColored color width
  = penDefault{ penColor = color, penWidth = width }

-- | A transparent pen.
penTransparent :: PenStyle
penTransparent
  = penDefault{ penKind = PenTransparent }

-- | Dash style
data DashStyle
  = DashDot
  | DashLong
  | DashShort
  | DashDotShort
  --  DashUser [Int]
  deriving (Eq,Show)

-- | Cap style
data CapStyle
  = CapRound          -- ^ End points are rounded
  | CapProjecting
  | CapButt
  deriving (Eq,Show)

-- | Join style.
data JoinStyle
  = JoinRound         -- ^ Corners are rounded
  | JoinBevel         -- ^ Corners are bevelled
  | JoinMiter         -- ^ Corners are blocked
  deriving (Eq,Show)

-- | Hatch style.
data HatchStyle
  = HatchBDiagonal    -- ^ Backward diagonal
  | HatchCrossDiag    -- ^ Crossed diagonal
  | HatchFDiagonal    -- ^ Forward diagonal
  | HatchCross        -- ^ Crossed orthogonal
  | HatchHorizontal   -- ^ Horizontal
  | HatchVertical     -- ^ Vertical
  deriving (Eq,Show)

-- | Brush style.
data BrushStyle
  = BrushStyle { brushKind :: !BrushKind, brushColor :: !Color }
  deriving (Eq,Show)

-- | Brush kind.
data BrushKind
  = BrushTransparent                            -- ^ No filling
  | BrushSolid                                  -- ^ Solid color
  | BrushHatch  { brushHatch  :: !HatchStyle }  -- ^ Hatch pattern
  | BrushStipple{ brushBitmap :: !(Bitmap ())}  -- ^ Bitmap pattern (on win95 only 8x8 bitmaps are supported)
  deriving (Eq,Show)


-- | Set a pen that is automatically deleted at the end of the computation.
dcWithPenStyle :: DC a -> PenStyle -> IO b -> IO b
dcWithPenStyle dc penStyle io
  = withPenStyle penStyle $ \pen ->
    dcWithPen dc pen io

-- | Set a pen that is used during a certain computation.
dcWithPen :: DC a -> Pen p -> IO b -> IO b
dcWithPen dc pen io
  = bracket  (do oldPen <- dcGetPen dc
                 dcSetPen dc pen
                 return oldPen)
             (\oldPen ->
              do dcSetPen dc oldPen   -- restore previous pen
                 penDelete oldPen)
             (const io)


-- | Set the current pen style. The text color is also adapted.
dcSetPenStyle :: DC a -> PenStyle -> IO ()
dcSetPenStyle dc penStyle
  = withPenStyle penStyle (dcSetPen dc)

-- | Get the current pen style.
dcGetPenStyle :: DC a -> IO PenStyle
dcGetPenStyle dc
  = do pen <- dcGetPen dc
       finalize (penDelete pen) $ 
         do penGetPenStyle pen

-- | Use a pen that is automatically deleted at the end of the computation.
withPenStyle :: PenStyle -> (Pen () -> IO a) -> IO a
withPenStyle penStyle f
  = do (pen,delete) <- penCreateFromStyle penStyle
       finally (f pen) delete

-- | Create a new pen from a 'PenStyle'. Returns both the pen and its deletion procedure.
penCreateFromStyle :: PenStyle -> IO (Pen (),IO ())
penCreateFromStyle penStyle
  = case penStyle of
      PenStyle PenTransparent color width cap join
        -> do pen <- penCreateFromStock 5 {- transparent -}
              return (pen,return ())
      PenStyle (PenDash DashShort) color 1 CapRound JoinRound  | color == black
        -> do pen <- penCreateFromStock 6 {- black dashed -}
              return (pen,return ())
      PenStyle PenSolid color 1 CapRound JoinRound
        -> case lookup color stockPens of
             Just idx -> do pen <- penCreateFromStock idx
                            return (pen,return ())
             Nothing  -> colorPen color 1 wxSOLID
      PenStyle PenSolid color width cap join
        -> colorPen color width wxSOLID
      PenStyle (PenDash dash) color width cap join
        -> case dash of
             DashDot  -> colorPen color width wxDOT
             DashLong -> colorPen color width wxLONG_DASH
             DashShort-> colorPen color width wxSHORT_DASH
             DashDotShort -> colorPen color width wxDOT_DASH
      PenStyle (PenStipple bitmap) color width cap join
        -> do pen <- penCreateFromBitmap bitmap width
              setCap pen
              setJoin pen
              return (pen,penDelete pen)
  where
    colorPen color width style
      = do pen <- penCreateFromColour color width style
           setCap pen
           setJoin pen
           return (pen,penDelete pen)

    setCap pen
      = case penCap penStyle of
          CapRound      -> return ()
          CapProjecting -> penSetCap pen wxCAP_PROJECTING
          CapButt       -> penSetCap pen wxCAP_BUTT

    setJoin pen
      = case penJoin penStyle of
          JoinRound     -> return ()
          JoinBevel     -> penSetJoin pen wxJOIN_BEVEL
          JoinMiter     -> penSetJoin pen wxJOIN_MITER

    stockPens
      = [(red,0),(cyan,1),(green,2)
        ,(black,3),(white,4)
        ,(grey,7),(lightgrey,9)
        ,(mediumgrey,8)
        ]

-- | Create a 'PenStyle' from a 'Pen'.
penGetPenStyle :: Pen a -> IO PenStyle
penGetPenStyle pen
  = do stl <- penGetStyle pen
       toPenStyle stl
  where
    toPenStyle stl
      | stl == wxTRANSPARENT      = return penTransparent
      | stl == wxSOLID            = toPenStyleWithKind PenSolid
      | stl == wxDOT              = toPenStyleWithKind (PenDash DashDot)
      | stl == wxLONG_DASH        = toPenStyleWithKind (PenDash DashLong)
      | stl == wxSHORT_DASH       = toPenStyleWithKind (PenDash DashShort)
      | stl == wxDOT_DASH         = toPenStyleWithKind (PenDash DashDotShort)
      | stl == wxSTIPPLE          = do stipple <- penGetStipple pen
                                       toPenStyleWithKind (PenStipple stipple)
      | stl == wxBDIAGONAL_HATCH  = toPenStyleWithKind (PenHatch HatchBDiagonal)
      | stl == wxCROSSDIAG_HATCH  = toPenStyleWithKind (PenHatch HatchCrossDiag)
      | stl == wxFDIAGONAL_HATCH  = toPenStyleWithKind (PenHatch HatchFDiagonal)
      | stl == wxCROSS_HATCH      = toPenStyleWithKind (PenHatch HatchCross)
      | stl == wxHORIZONTAL_HATCH = toPenStyleWithKind (PenHatch HatchHorizontal)
      | stl == wxVERTICAL_HATCH   = toPenStyleWithKind (PenHatch HatchVertical)
      | otherwise                 = toPenStyleWithKind PenSolid
      
    toPenStyleWithKind kind
      = do width  <- penGetWidth pen
           color  <- penGetColour pen
           cap    <- penGetCap pen
           join   <- penGetJoin pen
           return (PenStyle kind color width (toCap cap) (toJoin join))

    toCap cap
      | cap == wxCAP_PROJECTING = CapProjecting
      | cap == wxCAP_BUTT       = CapButt
      | otherwise               = CapRound

    toJoin join
      | join == wxJOIN_MITER    = JoinMiter
      | join == wxJOIN_BEVEL    = JoinBevel
      | otherwise               = JoinRound


           
{--------------------------------------------------------------------------------
  Brush
--------------------------------------------------------------------------------}
-- | Default brush (transparent, black).
brushDefault :: BrushStyle
brushDefault
  = BrushStyle BrushTransparent black


-- | Use a brush that is automatically deleted at the end of the computation.
dcWithBrushStyle :: DC a -> BrushStyle -> IO b -> IO b
dcWithBrushStyle dc brushStyle io
  = withBrushStyle brushStyle $ \brush ->
    dcWithBrush dc brush io

dcWithBrush :: DC b -> Brush a -> IO c -> IO c
dcWithBrush dc brush io
  = bracket  (do oldBrush <- dcGetBrush dc
                 dcSetBrush dc brush
                 return oldBrush)
             (\oldBrush ->
              do dcSetBrush dc oldBrush -- restore previous brush
                 brushDelete oldBrush)
             (const io)

-- | Set the brush style (and text background color) of a device context.
dcSetBrushStyle :: DC a -> BrushStyle -> IO ()
dcSetBrushStyle dc brushStyle
  = withBrushStyle brushStyle (dcSetBrush dc)
       
-- | Get the current brush of a device context.
dcGetBrushStyle :: DC a -> IO BrushStyle
dcGetBrushStyle dc
  = do brush <- dcGetBrush dc
       finalize (brushDelete brush) $
        do brushGetBrushStyle brush


-- | Use a brush that is automatically deleted at the end of the computation.
withBrushStyle :: BrushStyle -> (Brush () -> IO a) -> IO a
withBrushStyle brushStyle f
  = do (brush,delete) <- brushCreateFromStyle brushStyle
       finalize delete $ 
        do f brush 

-- | Create a new brush from a 'BrushStyle'. Returns both the brush and its deletion procedure.
brushCreateFromStyle :: BrushStyle -> IO (Brush (), IO ())
brushCreateFromStyle brushStyle
  = case brushStyle of
      BrushStyle BrushTransparent color
        -> do brush <- brushCreateFromStock 7   {- transparent brush -}
              return (brush,return ())
      BrushStyle BrushSolid color
        -> case lookup color stockBrushes of
             Just idx  -> do brush <- brushCreateFromStock idx
                             return (brush,return ())
             Nothing   -> colorBrush color wxSOLID

      BrushStyle (BrushHatch HatchBDiagonal) color   -> colorBrush color wxBDIAGONAL_HATCH
      BrushStyle (BrushHatch HatchCrossDiag) color   -> colorBrush color wxCROSSDIAG_HATCH
      BrushStyle (BrushHatch HatchFDiagonal) color   -> colorBrush color wxFDIAGONAL_HATCH
      BrushStyle (BrushHatch HatchCross) color       -> colorBrush color wxCROSS_HATCH
      BrushStyle (BrushHatch HatchHorizontal) color  -> colorBrush color wxHORIZONTAL_HATCH
      BrushStyle (BrushHatch HatchVertical) color    -> colorBrush color wxVERTICAL_HATCH
      BrushStyle (BrushStipple bitmap) color         -> do brush <- brushCreateFromBitmap bitmap
                                                           return (brush, brushDelete brush)
  where
    colorBrush color style
      = do brush <- brushCreateFromColour color style
           return (brush, brushDelete brush )

    stockBrushes
      = [(blue,0),(green,1),(white,2)
        ,(black,3),(grey,4),(lightgrey,6)
        ,(cyan,8),(red,9)
        ,(mediumgrey,5)
        ]

-- | Get the 'BrushStyle' of 'Brush'.
brushGetBrushStyle :: Brush a -> IO BrushStyle
brushGetBrushStyle brush
  = do stl   <- brushGetStyle brush
       kind  <- toBrushKind stl
       color <- brushGetColour brush
       return (BrushStyle kind color)
  where
    toBrushKind stl
      | stl == wxTRANSPARENT      = return BrushTransparent
      | stl == wxSOLID            = return BrushSolid
      | stl == wxSTIPPLE          = do stipple <- brushGetStipple brush
                                       return (BrushStipple stipple)
      | stl == wxBDIAGONAL_HATCH  = return (BrushHatch HatchBDiagonal)
      | stl == wxCROSSDIAG_HATCH  = return (BrushHatch HatchCrossDiag)
      | stl == wxFDIAGONAL_HATCH  = return (BrushHatch HatchFDiagonal)
      | stl == wxCROSS_HATCH      = return (BrushHatch HatchCross)
      | stl == wxHORIZONTAL_HATCH = return (BrushHatch HatchHorizontal)
      | stl == wxVERTICAL_HATCH   = return (BrushHatch HatchVertical)
      | otherwise                 = return BrushTransparent



{--------------------------------------------------------------------------------
  DC drawing state
--------------------------------------------------------------------------------}
-- | The drawing state (pen,brush,font,text color,text background color) of a device context.
data DrawState  = DrawState (Pen ()) (Brush ()) (Font ()) Color Color

-- | Run a computation after which the original drawing state of the 'DC' is restored.
dcEncapsulate :: DC a -> IO b -> IO b
dcEncapsulate dc io
  = bracket (dcGetDrawState dc)
            (\drawState ->
             do dcSetDrawState dc drawState
                drawStateDelete drawState)
            (const io)

-- | Get the drawing state. (Should be deleted with 'drawStateDelete').
dcGetDrawState     :: DC a -> IO DrawState
dcGetDrawState dc
  = do pen   <- dcGetPen dc
       brush <- dcGetBrush dc
       font  <- dcGetFont dc
       textc <- dcGetTextForeground dc
       backc <- dcGetTextBackground dc
       return (DrawState pen brush font textc backc)

-- | Set the drawing state.
dcSetDrawState  :: DC a -> DrawState -> IO ()
dcSetDrawState dc (DrawState pen brush font textc backc)
  = do dcSetPen dc pen
       dcSetBrush dc brush
       dcSetFont dc font
       dcSetTextBackground dc backc
       dcSetTextForeground dc textc

-- | Release the resources associated with a drawing state.
drawStateDelete :: DrawState -> IO ()
drawStateDelete (DrawState pen brush font _ _)
  = do penDelete pen
       brushDelete brush
       fontDelete font

{--------------------------------------------------------------------------------
  DC utils
--------------------------------------------------------------------------------}

-- | Draw connected lines.
drawLines :: DC a -> [Point] -> IO ()
drawLines dc []  = return ()
drawLines dc ps
  = withArray xs $ \pxs ->
    withArray ys $ \pys ->
    dcDrawLines dc n pxs pys (pt 0 0)
  where
    n  = length ps
    xs = map px ps
    ys = map py ps


-- | Draw a polygon. The polygon is filled with the odd-even rule.
drawPolygon :: DC a -> [Point] -> IO ()
drawPolygon dc []  = return ()
drawPolygon dc ps
  = withArray xs $ \pxs ->
    withArray ys $ \pys ->
    dcDrawPolygon dc n pxs pys (pt 0 0) wxODDEVEN_RULE
  where
    n  = length ps
    xs = map px ps
    ys = map py ps

-- | Gets the dimensions of the string using the currently selected font.
getTextExtent :: DC a -> String -> IO Size
getTextExtent dc txt
  = do (sz,_,_) <- getFullTextExtent dc txt
       return sz

-- | Gets the dimensions of the string using the currently selected font.
-- Takes text string to measure, and returns the size, /descent/ and /external leading/.
-- Descent is the dimension from the baseline of the font to the bottom of the descender
-- , and external leading is any extra vertical space added to the font by the font designer (is usually zero).
getFullTextExtent :: DC a -> String -> IO (Size,Int,Int)
getFullTextExtent dc txt
  = alloca $ \px ->
    alloca $ \py ->
    alloca $ \pd ->
    alloca $ \pe ->
    do dcGetTextExtent dc txt px py pd pe objectNull
       x <- peek px
       y <- peek py
       d <- peek pd
       e <- peek pe
       return (sz (fromCInt x) (fromCInt y), fromCInt d, fromCInt e)


{--------------------------------------------------------------------------------
  Double buffering
--------------------------------------------------------------------------------}

-- | Use double buffering to draw to a 'DC' -- reduces flicker. Note that
-- the 'windowOnPaint' handler can already take care of buffering automatically.
-- The rectangle argument is normally the view rectangle ('windowGetViewRect').
-- Uses a 'MemoryDC' to draw into memory first and than blit the result to
-- the device context. The memory area allocated is the minimal size necessary
-- to accomodate the rectangle, but is re-allocated on each invokation.
dcBuffer :: DC a -> Rect -> (DC () -> IO ()) -> IO ()
dcBuffer dc r draw
  = dcBufferWithRef dc Nothing r draw

-- | Optimized double buffering. Takes a possible reference to a bitmap. If it is
-- 'Nothing', a new bitmap is allocated everytime. Otherwise, the reference is used
-- to re-use an allocated bitmap if possible.
dcBufferWithRef :: DC a -> Maybe (Var (Bitmap ())) -> Rect -> (DC () -> IO ()) -> IO ()
dcBufferWithRef dc mbVar r draw
  | rectSize r == sizeZero = return ()
dcBufferWithRef dc mbVar r draw
  = bracket (initBitmap)
            (doneBitmap)
            (\bitmap ->
             if (bitmap==objectNull)
              then drawUnbuffered
              else bracket (do p <- memoryDCCreateCompatible dc; return (objectCast p))
                           (\memdc -> when (memdc/=objectNull) (memoryDCDelete memdc))
                           (\memdc -> if (memdc==objectNull)
                                       then drawUnbuffered
                                       else do memoryDCSelectObject memdc bitmap
                                               drawBuffered memdc
                                               memoryDCSelectObject memdc nullBitmap
                           )
            )
    where
     initBitmap
       = case mbVar of
           Nothing  -> bitmapCreateEmpty (rectSize r) (-1)
           Just v   -> do bitmap <- varGet v
                          size   <- if (bitmap==objectNull)
                                     then return sizeZero
                                     else do bw <- bitmapGetWidth bitmap
                                             bh <- bitmapGetHeight bitmap
                                             return (Size bw bh)
                          -- re-use the bitmap if possible
                          if (sizeEncloses size (rectSize r) && bitmap /= objectNull)
                            then return bitmap
                            else do when (bitmap/=objectNull) (bitmapDelete bitmap)
                                    varSet v objectNull
                                    -- new size a bit larger to avoid multiple reallocs
                                    let (Size w h) = rectSize r
                                        neww       = div (w*105) 100
                                        newh       = div (h*105) 100
                                    bm <- bitmapCreateEmpty (sz neww newh) (-1)
                                    varSet v bm
                                    return bm

     doneBitmap bitmap
       = case mbVar of
           Nothing -> when (bitmap/=objectNull) (bitmapDelete bitmap)
           Just v  -> return ()


     drawUnbuffered
       = draw (objectCast dc) -- down cast

     drawBuffered memdc
      = do -- set the device origin for scrolled windows
           dcSetDeviceOrigin memdc (pointFromVec (vecNegate (vecFromPoint (topLeft r))))
           dcSetClippingRegion memdc r
           bracket (dcGetBackground dc)
                   (\brush -> do dcSetBrush memdc nullBrush
                                 brushDelete brush)
                   (\brush -> do -- set the background to the owner brush
                                 dcSetBackground memdc brush
                                 dcSetBrush memdc brush
                                 dcWithPenStyle memdc penTransparent (dcDrawRectangle memdc r)
                                 -- and finally do the drawing!
                                 draw (objectCast memdc) -- down cast
                   )
           -- blit the memdc into the owner dc.
           dcBlit dc r memdc (topLeft r) wxCOPY False
           return ()