{-# OPTIONS -cpp -fglasgow-exts -#include "wxc.h" #-}
-----------------------------------------------------------------------------------------
{-| Module      :  Types
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Basic types and marshaling code for the wxWindows C library.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.WxcTypes(
            -- * Object types
              Object, objectNull, objectIsNull, objectCast
            , Managed, managedNull, managedIsNull, managedCast, createManaged, withManaged, managedTouch

            -- * Type synonyms
            , Id
            , Style
            , EventId

            -- * Basic types
            , intFromBool, boolFromInt

            -- ** Point
            , Point(Point,pointX,pointY), point, pt, pointFromVec, pointFromSize, pointZero, pointNull

            -- ** Size
            , Size(Size,sizeW,sizeH), size, sz, sizeFromPoint, sizeFromVec, sizeZero, sizeNull

            -- ** Vector
            , Vector(Vector,vecX,vecY), vector, vec, vecFromPoint, vecFromSize, vecZero, vecNull

            -- * Rectangle
            , Rect(Rect,rectLeft,rectTop,rectWidth,rectHeight)
            , rectTopLeft, rectTopRight, rectBottomLeft, rectBottomRight, rectBottom, rectRight
            , rect, rectBetween, rectFromSize, rectZero, rectNull, rectSize, rectIsEmpty

            -- ** Color
            , Color, rgb, colorRGB, colorRed, colorGreen, colorBlue, colorOk

            -- * Marshalling
            -- ** Basic types
            , withPointResult, toCIntPointX, toCIntPointY, fromCPoint, withCPoint
            , withSizeResult, toCIntSizeW, toCIntSizeH, fromCSize, withCSize
            , withVectorResult, toCIntVectorX, toCIntVectorY, fromCVector, withCVector
            , withRectResult, toCIntRectX, toCIntRectY, toCIntRectW, toCIntRectH, fromCRect, withCRect
            , withArrayString, withArrayInt, withArrayObject
            , withArrayIntResult, withArrayStringResult, withArrayObjectResult

            , Colour, ColourObject
            , colourFromColor, colorFromColour
            , colourCreate, colourCreateRGB, colourDelete, colourRed, colourGreen, colourBlue

            -- ** Managed object types
            , managedAddFinalizer
            , withRefColour, withManagedColourResult, withManagedColour
            , withRefBitmap
            , withRefCursor
            , withRefIcon
            , withRefPen
            , withRefBrush
            , withRefFont
            , withRefDateTime
            , withRefListItem
            , withRefTreeItemId
            , withRefFontData
            , withRefPrintData
            , withRefPageSetupDialogData
            , withRefPrintDialogData
            , withRefGridCellCoordsArray


            -- ** Primitive types
            -- *** CString
            , CString, withCString, withStringResult
            -- *** CInt
            , CInt, toCInt, fromCInt, withIntResult
            -- *** CChar
            , CChar, toCChar, fromCChar, withCharResult
            -- *** CBool
            , CBool, toCBool, fromCBool, withBoolResult
            -- ** Pointers
            , Ptr, ptrNull, ptrIsNull, ptrCast, ForeignPtr, FunPtr, toCFunPtr
            ) where

import System.IO.Unsafe( unsafePerformIO )
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

{- note: for GHC 5.04, replace the following two imports by "import Foreign.ForeignPtr" -}
import Foreign.ForeignPtr hiding (newForeignPtr,addForeignPtrFinalizer)
import Foreign.Concurrent



{-----------------------------------------------------------------------------------------
    Objects
-----------------------------------------------------------------------------------------}
-- | An @Id@ is used to identify objects during event handling.
type Id = Int

-- | An @EventId@ is identifies specific events.
type EventId = Int

-- | A @Style@ is normally used as a flag mask to specify some window style
type Style = Int


{- | An @Object a@ is a pointer to an object of type @a@. The @a@ parameter is used
   to encode the inheritance relation. When the type parameter is unit @()@, it denotes
   an object of exactly that class, when the parameter is a type variable @a@, it
   specifies an object that is at least an instance of that class. For example in 
   wxWindows, we have the following class hierarchy:

   > EvtHandler
   >   |- Window
   >        |- Frame
   >        |- Control
   >            |- Button
   >            |- Radiobox

   In wxHaskell, all the creation functions will return objects of exactly that
   class and use the @()@ type:

   > frameCreate :: Window a -> ... -> IO (Frame ())
   > buttonCreate :: Window a -> ... -> IO (Button ())
   > ...

   In contrast, all the /this/ (or /self/) pointers of methods can take objects
   of any instance of that class and have a type variable, for example:

   > windowSetClientSize :: Window a -> Size -> IO ()
   > controlSetLabel     :: Control a -> String -> IO ()
   > buttonSetDefault    :: Button a -> IO ()

   This means that we can use @windowSetClientSize@ on any window, including
   buttons and frames, but we can only use @controlSetLabel@ on controls, not
   includeing frames. 

   In wxHaskell, this works since a @Frame ()@ is actually a type synonym for
   @Window (CFrame ())@ (where @CFrame@ is an abstract data type). We can thus
   pass a value of type @Frame ()@ to anything that expects some @Window a@.
   For a button this works too, as it is a synonym for @Control (CButton ())@
   which is in turn a synonym for @Window (CControl (CButton ()))@. Note that
   we can\'t pass a frame to something that expects a value of type @Control a@.
   Of course, a @Window a@ is actually a type synonym for @EvtHandler (CWindow a)@.
   If you study the documentation in "Graphics.UI.WXH.WxcClasses" closely, you
   can discover where this chain ends :-).  

   Objects are not automatically deleted. Normally you can use a delete function
   like @windowDelete@ to delete an object. However, almost all objects in the
   wxWindows library are automatically deleted by the library. The only objects
   that should be used with care are resources as bitmaps, fonts and brushes.
-}
type Object a   = Ptr a

-- | A null object. Use with care.
objectNull :: Object a
objectNull
  = nullPtr

-- | Test for null object.
objectIsNull :: Object a -> Bool
objectIsNull p
  = (p == objectNull)

-- | Cast an object to another type. Use with care.
objectCast :: Object a -> Object b
objectCast obj
  = castPtr obj

{-----------------------------------------------------------------------------------------
  Point
-----------------------------------------------------------------------------------------}
-- | A point has an x and y coordinate. Coordinates are normally relative to the
-- upper-left corner of their view frame, where a positive x goes to the right and
-- a positive y to the bottom of the view.
data Point  = Point
        { pointX :: !Int -- ^ x component of a point.
        , pointY :: !Int -- ^ y component of a point.
        }
        deriving (Eq,Show)

-- | Construct a point.
point :: Int -> Int -> Point
point x y  = Point x y

-- | Shorter function to construct a point.
pt :: Int -> Int -> Point
pt x y  = Point x y

pointFromVec :: Vector -> Point
pointFromVec (Vector x y)
  = Point x y

pointFromSize :: Size -> Point
pointFromSize (Size w h)
  = Point w h

-- | Point at the origin.
pointZero :: Point
pointZero
  = Point 0 0

-- | A `null' point is not a legal point (x and y are -1) and can be used for some
-- wxWindows functions to select a default point.
pointNull :: Point
pointNull
  = Point (-1) (-1)

-- marshalling
withCPoint :: Point -> (CInt -> CInt -> IO a) -> IO a
withCPoint (Point x y) f
  = f (toCInt x) (toCInt y)

withPointResult :: (Ptr CInt -> Ptr CInt -> IO ()) -> IO Point
withPointResult f
  = alloca $ \px ->
    alloca $ \py ->
    do f px py
       x <- peek px
       y <- peek py
       return (fromCPoint x y)

toCIntPointX, toCIntPointY :: Point -> CInt
toCIntPointX (Point x y)  = toCInt x
toCIntPointY (Point x y)  = toCInt y

fromCPoint :: CInt -> CInt -> Point
fromCPoint x y
  = Point (fromCInt x) (fromCInt y)


{-----------------------------------------------------------------------------------------
  Size
-----------------------------------------------------------------------------------------}
-- | A @Size@ has a width and height.
data Size   = Size
        { sizeW :: !Int -- ^ the width  of a size
        , sizeH :: !Int -- ^ the height of a size
        }
        deriving (Eq,Show)

-- | Construct a size from a width and height.
size :: Int -> Int -> Size
size w h
  = Size w h

-- | Short function to construct a size
sz :: Int -> Int -> Size
sz w h
  = Size w h

sizeFromPoint :: Point -> Size
sizeFromPoint (Point x y)
  = Size x y

sizeFromVec   :: Vector -> Size
sizeFromVec (Vector x y)
  = Size x y

sizeZero :: Size
sizeZero
  = Size 0 0

-- | A `null' size is not a legal size (width and height are -1) and can be used for some
-- wxWindows functions to select a default size.
sizeNull :: Size
sizeNull
  = Size (-1) (-1)

-- marshalling
withCSize :: Size -> (CInt -> CInt -> IO a) -> IO a
withCSize (Size w h) f
  = f (toCInt w) (toCInt h)

withSizeResult :: (Ptr CInt -> Ptr CInt -> IO ()) -> IO Size
withSizeResult f
  = alloca $ \cw ->
    alloca $ \ch ->
    do f cw ch
       w <- peek cw
       h <- peek ch
       return (fromCSize w h)

fromCSize :: CInt -> CInt -> Size
fromCSize w h
  = Size (fromCInt w) (fromCInt h)

toCIntSizeW, toCIntSizeH :: Size -> CInt
toCIntSizeW (Size w h)  = toCInt w
toCIntSizeH (Size w h)  = toCInt h

{-----------------------------------------------------------------------------------------
  Vector
-----------------------------------------------------------------------------------------}
-- | A vector with an x and y delta.
data Vector = Vector
        { vecX :: !Int -- ^ delta-x component of a vector
        , vecY :: !Int -- ^ delta-y component of a vector
        }
        deriving (Eq,Show)

-- | Construct a vector.
vector :: Int -> Int -> Vector
vector dx dy  = Vector dx dy

-- | Short function to construct a vector.
vec :: Int -> Int -> Vector
vec dx dy  = Vector dx dy

-- | A zero vector
vecZero :: Vector
vecZero
  = Vector 0 0

-- | A `null' vector has a delta x and y of -1 and can be used for some
-- wxWindows functions to select a default vector.
vecNull :: Vector
vecNull
  = Vector (-1) (-1)

vecFromPoint :: Point -> Vector
vecFromPoint (Point x y)
  = Vector x y

vecFromSize :: Size -> Vector
vecFromSize (Size w h)
  = Vector w h


-- marshalling
withCVector :: Vector -> (CInt -> CInt -> IO a) -> IO a
withCVector (Vector x y) f
  = f (toCInt x) (toCInt y)

withVectorResult :: (Ptr CInt -> Ptr CInt -> IO ()) -> IO Vector
withVectorResult f
  = alloca $ \px ->
    alloca $ \py ->
    do f px py
       x <- peek px
       y <- peek py
       return (fromCVector x y)

toCIntVectorX, toCIntVectorY :: Vector -> CInt
toCIntVectorX (Vector x y)  = toCInt x
toCIntVectorY (Vector x y)  = toCInt y

fromCVector :: CInt -> CInt -> Vector
fromCVector x y
  = Vector (fromCInt x) (fromCInt y)


{-----------------------------------------------------------------------------------------
  Rectangle
-----------------------------------------------------------------------------------------}
-- | A rectangle is defined by the left x coordinate, the top y coordinate,
-- the width and the height.
data Rect   = Rect
        { rectLeft   :: !Int
        , rectTop    :: !Int
        , rectWidth  :: !Int
        , rectHeight :: !Int
        }
        deriving (Eq,Show)


rectTopLeft, rectTopRight, rectBottomLeft, rectBottomRight :: Rect -> Point
rectTopLeft     (Rect l t w h)  = Point l t
rectTopRight    (Rect l t w h)  = Point (l+w) t
rectBottomLeft  (Rect l t w h)  = Point l (t+h)
rectBottomRight (Rect l t w h)  = Point (l+w) (t+h)

rectBottom, rectRight :: Rect -> Int
rectBottom (Rect x y w h)  = y + h
rectRight  (Rect x y w h)  = x + w

-- | Create a rectangle at a certain (upper-left) point with a certain size.
rect :: Point -> Size -> Rect
rect (Point x y) (Size w h)
  = Rect x y w h

-- | Construct a (positive) rectangle between two (arbitrary) points.
rectBetween :: Point -> Point -> Rect
rectBetween (Point x0 y0) (Point x1 y1)
  = Rect (min x0 x1) (min y0 y1) (abs (x1-x0)) (abs (y1-y0))

-- | An empty rectangle at (0,0).
rectZero :: Rect
rectZero
  = Rect 0 0 0 0

-- | An `null' rectangle is not a valid rectangle (@Rect -1 -1 -1 -1@) but can
-- used for some wxWindows functions to select a default rectangle. (i.e. 'frameCreate').
rectNull :: Rect
rectNull
  = Rect (-1) (-1) (-1) (-1)

-- | Get the size of a rectangle.
rectSize :: Rect -> Size
rectSize (Rect l t w h)
  = Size w h

-- | Create a rectangle of a certain size with the upper-left corner at ('pt' 0 0).
rectFromSize :: Size -> Rect
rectFromSize (Size w h)
  = Rect 0 0 w h

rectIsEmpty :: Rect -> Bool
rectIsEmpty (Rect l t w h)
  = (w==0 && h==0)



-- marshalling
withCRect :: Rect -> (CInt -> CInt -> CInt -> CInt -> IO a) -> IO a
withCRect (Rect x0 y0 x1 y1) f
  = f (toCInt (x0)) (toCInt (y0)) (toCInt (x1)) (toCInt (y1))

withRectResult :: (Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()) -> IO Rect
withRectResult f
  = alloca $ \cx ->
    alloca $ \cy ->
    alloca $ \cw ->
    alloca $ \ch ->
    do f cx cy cw ch
       x <- peek cx
       y <- peek cy
       w <- peek cw
       h <- peek ch
       return (fromCRect x y w h)

fromCRect :: CInt -> CInt -> CInt -> CInt -> Rect
fromCRect x y w h
  = Rect (fromCInt x) (fromCInt y) (fromCInt w) (fromCInt h)

toCIntRectX, toCIntRectY, toCIntRectW, toCIntRectH :: Rect -> CInt
toCIntRectX (Rect x y w h)  = toCInt x
toCIntRectY (Rect x y w h)  = toCInt y
toCIntRectW (Rect x y w h)  = toCInt w
toCIntRectH (Rect x y w h)  = toCInt h

{-----------------------------------------------------------------------------------------
  CInt
-----------------------------------------------------------------------------------------}
withIntResult :: IO CInt -> IO Int
withIntResult io
  = do x <- io
       return (fromCInt x)

toCInt :: Int -> CInt
toCInt i = fromIntegral i

fromCInt :: CInt -> Int
fromCInt ci
  = fromIntegral ci

{-----------------------------------------------------------------------------------------
  CBool
-----------------------------------------------------------------------------------------}
type CBool  = CInt

toCBool :: Bool -> CBool
toCBool b     = toCInt (if b then 1 else 0)

withBoolResult :: IO CBool -> IO Bool
withBoolResult io
  = do x <- io
       return (fromCBool x)

fromCBool :: CBool -> Bool
fromCBool cb
  = (cb /= 0)


intFromBool :: Bool -> Int
intFromBool b
  = if b then 1 else 0

boolFromInt :: Int -> Bool
boolFromInt i
  = (i/=0)

{-----------------------------------------------------------------------------------------
  CString
-----------------------------------------------------------------------------------------}
withStringResult :: (Ptr CChar -> IO CInt) -> IO String
withStringResult f
  = do len <- f nullPtr
       if (len<=0)
        then return ""
        else withCString (replicate (fromCInt len) ' ') $ \cstr ->
             do f cstr
                peekCString cstr

{-----------------------------------------------------------------------------------------
  Arrays
-----------------------------------------------------------------------------------------}
withArrayStringResult :: (Ptr (Ptr CChar) -> IO CInt) -> IO [String]
withArrayStringResult f
  = do clen <- f nullPtr
       let len = fromCInt clen
       if (len <= 0)
        then return []
        else allocaArray len $ \carr ->
             do f carr
                arr <- peekArray len carr
                mapM peekCString arr

withArrayIntResult :: (Ptr CInt -> IO CInt) -> IO [Int]
withArrayIntResult f
  = do clen <- f nullPtr
       let len = fromCInt clen
       if (len <= 0)
        then return []
        else allocaArray len $ \carr ->
             do f carr
                xs <- peekArray len carr
                return (map fromCInt xs)

withArrayObjectResult :: (Ptr (Ptr a) -> IO CInt) -> IO [Ptr a]
withArrayObjectResult f
  = do clen <- f nullPtr
       let len = fromCInt clen
       if (len <= 0)
        then return []
        else allocaArray len $ \carr ->
             do f carr
                peekArray len carr

withArrayString :: [String] -> (CInt -> Ptr CString -> IO a) -> IO a
withArrayString xs f
  = withCStrings xs [] $ \cxs ->
    withArray0 ptrNull cxs $ \carr ->
    f (toCInt len) carr
  where
    len = length xs

    withCStrings [] cxs f
      = f (reverse cxs)
    withCStrings (x:xs) cxs f
      = withCString x $ \cx ->
        withCStrings xs (cx:cxs) f

withArrayInt :: [Int] -> (CInt -> Ptr CInt -> IO a) -> IO a
withArrayInt xs f
  = withArray0 0 (map toCInt xs) $ \carr ->
    f (toCInt (length xs)) carr

withArrayObject :: [Ptr a] -> (CInt -> Ptr (Ptr a) -> IO b) -> IO b
withArrayObject xs f
  = withArray0 ptrNull xs $ \carr ->
    f (toCInt (length xs)) carr

{-----------------------------------------------------------------------------------------
  CCHar
-----------------------------------------------------------------------------------------}
toCChar :: Char -> CChar
toCChar c
  = fromIntegral (fromEnum c)

withCharResult :: IO CChar -> IO Char
withCharResult io
  = do x <- io
       return (fromCChar x)

fromCChar :: CChar -> Char
fromCChar cc
  = toEnum (fromIntegral cc)

{-----------------------------------------------------------------------------------------
  CFunPtr
-----------------------------------------------------------------------------------------}
toCFunPtr :: FunPtr a -> Ptr a
toCFunPtr fptr
  = castFunPtrToPtr fptr

-- | Null pointer, use with care.
ptrNull :: Ptr a
ptrNull
  = nullPtr

-- | Test for null.
ptrIsNull :: Ptr a -> Bool
ptrIsNull p
  = (p == ptrNull)

-- | Cast a pointer type, use with care.
ptrCast :: Ptr a -> Ptr b
ptrCast p
  = castPtr p

{-----------------------------------------------------------------------------------------
  Marshalling of classes that are managed
-----------------------------------------------------------------------------------------}
-- | A @Managed a@ is a pointer to an object of type @a@, just like 'Object'. However,
-- managed objects are automatically deleted when garbage collected. This is used for
-- certain classes that are not managed by the wxWindows library, like 'Bitmap's
type Managed a  = ForeignPtr a

-- | Create a managed object. Takes a finalizer as argument. This is normally a
-- a delete function like 'windowDelete'.
createManaged :: IO () -> Object a -> IO (Managed a)
createManaged final obj
  = newForeignPtr obj final

-- | Add an extra finalizer to a managed object.
managedAddFinalizer :: IO () -> Managed a -> IO ()
managedAddFinalizer io managed
  = addForeignPtrFinalizer managed io

-- | Do something with the object from a managed object.
withManaged :: Managed a -> (Object a -> IO b) -> IO b
withManaged fptr f
  = withForeignPtr fptr f


-- | Keep a managed object explicitly alive.
managedTouch :: Managed a -> IO ()
managedTouch fptr
  = touchForeignPtr fptr

-- | A null pointer, use with care.
{-# NOINLINE managedNull #-}
managedNull :: Managed a
managedNull
  = unsafePerformIO (createManaged (return ()) objectNull)

-- | Test for null.
managedIsNull :: Managed a -> Bool
managedIsNull managed
  = (managed == managedNull)

-- | Cast a managed object, use with care.
managedCast :: Managed a -> Managed b
managedCast fptr
  = castForeignPtr fptr


{-----------------------------------------------------------------------------------------
  Classes assigned by value.
-----------------------------------------------------------------------------------------}
assignRef :: IO (Object a) -> (Object a -> IO ()) -> IO (Object a)
assignRef create f
  = do p <- create
       f p
       return p

withRefBitmap :: (Object a -> IO ()) -> IO (Object a)
withRefBitmap f
  = assignRef wxBitmap_Create  f
foreign import ccall "wxBitmap_CreateDefault" wxBitmap_Create :: IO (Object a)

withRefCursor :: (Object a -> IO ()) -> IO (Object a)
withRefCursor f
  = assignRef (wx_Cursor_CreateFromStock 1)  f
foreign import ccall "Cursor_CreateFromStock" wx_Cursor_CreateFromStock :: CInt -> IO (Object a)

withRefIcon :: (Object a -> IO ()) -> IO (Object a)
withRefIcon f
  = assignRef wxIcon_Create  f
foreign import ccall "wxIcon_CreateDefault" wxIcon_Create :: IO (Object a)


withRefFont :: (Object a -> IO ()) -> IO (Object a)
withRefFont f
  = assignRef wxFont_Create  f
foreign import ccall "wxFont_CreateDefault" wxFont_Create :: IO (Object a)


withRefPen :: (Object a -> IO ()) -> IO (Object a)
withRefPen f
  = assignRef wxPen_Create  f
foreign import ccall "wxPen_CreateDefault" wxPen_Create :: IO (Object a)


withRefBrush :: (Object a -> IO ()) -> IO (Object a)
withRefBrush f
  = assignRef wxBrush_Create  f
foreign import ccall "wxBrush_CreateDefault" wxBrush_Create :: IO (Object a)

withRefDateTime :: (Object a -> IO ()) -> IO (Object a)
withRefDateTime f
  = assignRef wxDateTime_Create  f
foreign import ccall "wxDateTime_Create" wxDateTime_Create :: IO (Object a)

withRefListItem :: (Object a -> IO ()) -> IO (Object a)
withRefListItem f
  = assignRef wxListItem_Create  f
foreign import ccall "wxListItem_Create" wxListItem_Create :: IO (Object a)

withRefTreeItemId :: (Object a -> IO ()) -> IO (Object a)
withRefTreeItemId f
  = assignRef wxTreeItemId_Create  f
foreign import ccall "wxTreeItemId_Create" wxTreeItemId_Create :: IO (Object a)


withRefFontData :: (Object a -> IO ()) -> IO (Object a)
withRefFontData f
  = assignRef wxFontData_Create  f
foreign import ccall "wxFontData_Create" wxFontData_Create :: IO (Object a)

withRefPrintData :: (Object a -> IO ()) -> IO (Object a)
withRefPrintData f
  = assignRef wxPrintData_Create  f
foreign import ccall "wxPrintData_Create" wxPrintData_Create :: IO (Object a)

withRefPrintDialogData :: (Object a -> IO ()) -> IO (Object a)
withRefPrintDialogData f
  = assignRef wxPrintDialogData_Create  f
foreign import ccall "wxPrintDialogData_CreateDefault" wxPrintDialogData_Create :: IO (Object a)

withRefPageSetupDialogData :: (Object a -> IO ()) -> IO (Object a)
withRefPageSetupDialogData f
  = assignRef wxPageSetupDialogData_Create  f
foreign import ccall "wxPageSetupDialogData_Create" wxPageSetupDialogData_Create :: IO (Object a)

withRefGridCellCoordsArray :: (Object a -> IO ()) -> IO (Object a)
withRefGridCellCoordsArray f
  = assignRef wxGridCellCoordsArray_Create  f
foreign import ccall "wxGridCellCoordsArray_Create" wxGridCellCoordsArray_Create :: IO (Object a)

{-----------------------------------------------------------------------------------------
  Color
-----------------------------------------------------------------------------------------}
-- | An abstract data type to define colors.
data Color = Color Int Int Int deriving Eq

instance Show Color where
  showsPrec d c
    = showParen (d > 0) (showString "rgb(" . shows (colorRed   c) .
                          showChar   ','   . shows (colorGreen c) .
                          showChar   ','   . shows (colorBlue  c) .
                          showChar   ')' )

-- | Create a color from a red\/green\/blue triple.
colorRGB :: Int -> Int -> Int -> Color
colorRGB r g b = Color r g b

-- | Create a color from a red\/green\/blue triple.
rgb :: Int -> Int -> Int -> Color
rgb r g b = Color r g b

-- | Returns a red color component
colorRed   :: Color -> Int
colorRed   (Color r g b) = r

-- | Returns a green color component
colorGreen :: Color -> Int
colorGreen (Color r g b) = g

-- | Returns a blue color component
colorBlue  :: Color -> Int
colorBlue  (Color r g b) = b

-- | This is an illegal color, corresponding to @nullColour@.
colorNull :: Color
colorNull
  = rgb (-1) (-1) (-1)

-- | Check of a color is valid (@Colour::Ok@)
colorOk :: Color -> Bool
colorOk (Color r g b)
  = bound r && bound g && bound b
  where
    bound x = (x >= 0) && (x <= 255)


-- marshalling
type Colour a        = Managed (CColour a)
type ColourObject a  = Object (CColour a)
data CColour a       = CColour

withRefColour :: (ColourObject () -> IO ()) -> IO Color
withRefColour f
  = withManagedColourResult $
    assignRef colourCreate f

withManagedColourResult :: IO (ColourObject a) -> IO Color
withManagedColourResult io
  = do pcolour <- io
       color <- do ok <- colourOk pcolour
                   if (ok==0)
                    then return colorNull
                    else do r <- colourRed pcolour
                            g <- colourGreen pcolour
                            b <- colourBlue pcolour
                            return (Color (fromIntegral r) (fromIntegral g) (fromIntegral b))
       colourDelete pcolour
       return color

withManagedColour :: Color -> (ColourObject () -> IO b) -> IO b
withManagedColour (Color r g b) f
  = do pcolour <- colourCreateRGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
       x <- f pcolour
       colourDelete pcolour
       return x

colourFromColor :: Color -> IO (Colour ())
colourFromColor color@(Color r g b)
  = if (colorOk color)
     then do pcolour <- colourCreateRGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
             newForeignPtr pcolour (colourDelete pcolour)
     else do pcolour <- colourNull
             newForeignPtr pcolour (return ())

colorFromColour :: Colour a -> IO Color
colorFromColour c
  = withManaged c $ \pcolour ->
    do ok <- colourOk pcolour
       if (ok==0)
        then return colorNull
        else do r <- colourRed pcolour
                g <- colourGreen pcolour
                b <- colourBlue pcolour
                return (Color (fromIntegral r) (fromIntegral g) (fromIntegral b))


foreign import ccall "wxColour_CreateEmpty" colourCreate    :: IO (ColourObject ())
foreign import ccall "wxColour_CreateRGB" colourCreateRGB :: CUChar -> CUChar -> CUChar -> IO (ColourObject ())
foreign import ccall "wxColour_Delete" colourDelete   :: ColourObject a -> IO ()
foreign import ccall "wxColour_Red"   colourRed       :: ColourObject a -> IO CUChar
foreign import ccall "wxColour_Green" colourGreen     :: ColourObject a -> IO CUChar
foreign import ccall "wxColour_Blue"  colourBlue      :: ColourObject a -> IO CUChar
foreign import ccall "wxColour_Ok"    colourOk        :: ColourObject a -> IO CInt
foreign import ccall "Null_Colour"    colourNull      :: IO (ColourObject ())