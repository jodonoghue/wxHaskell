{-# OPTIONS -fglasgow-exts -#include "wxc.h" #-}
-----------------------------------------------------------------------------------------
{-| Module      :  Types
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Basic types and operations.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Types(
            -- * Objects
              ( # )
            , Object, objectNull, objectIsNull, objectCast
            , Managed, managedNull, managedIsNull, managedCast, createManaged, withManaged, managedTouch

            -- * Identifiers
            , Id, idAny, idCreate

            -- * Bits
            , (.+.), (.-.)
            , bits
            , bitsSet

            -- * Control
            , unitIO, bracket, bracket_, finally, finalize, when

            -- * Variables
            , Var, varCreate, varGet, varSet, varUpdate, varSwap

            -- * Misc.
            , Style
            , EventId
            , TreeItem

            -- * Basic types

            -- ** Booleans
            , boolFromInt, intFromBool

            -- ** Colors
            , Color, rgb, colorRGB, colorRed, colorGreen, colorBlue
            , black, darkgrey, dimgrey, mediumgrey, grey, lightgrey, white
            , red, green, blue
            , cyan, magenta, yellow

            -- ** Points
            , Point(Point,pointX,pointY), point, pt, pointFromVec, pointFromSize, pointZero, pointNull
            , pointMove, pointMoveBySize, pointAdd, pointSub, pointScale

            -- ** Sizes
            , Size(Size,sizeW,sizeH), size, sz, sizeFromPoint, sizeFromVec, sizeZero, sizeNull, sizeEncloses
            , sizeMin, sizeMax

            -- ** Vectors
            , Vector(Vector,vecX,vecY), vector, vec, vecFromPoint, vecFromSize, vecZero, vecNull
            , vecNegate, vecOrtogonal, vecAdd, vecSub, vecScale, vecBetween, vecLength

            -- ** Rectangles
            , Rect(Rect,rectLeft,rectTop,rectWidth,rectHeight)
            , rectTopLeft, rectTopRight, rectBottomLeft, rectBottomRight, rectBottom, rectRight
            , rect, rectBetween, rectFromSize, rectZero, rectNull, rectSize, rectIsEmpty
            , rectContains, rectMoveTo, rectFromPoint, rectCentralPoint, rectCentralRect, rectStretchTo
            , rectMove, rectOverlaps, rectsDiff, rectUnion, rectOverlap, rectUnions

            ) where

import List( (\\) )
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import System.IO.Unsafe( unsafePerformIO )

-- utility
import Data.Bits
import Data.IORef
import qualified Control.Exception as CE
import qualified Monad as M


infixl 5 .+.
infixl 5 .-.
infix 5 #

-- | Reverse application. Useful for an object oriented style of programming.
--
-- > (frame # frameSetTitle) "hi"
--
( # ) :: obj -> (obj -> a) -> a
object # method   = method object


{--------------------------------------------------------------------------------
  Bitmasks
--------------------------------------------------------------------------------}
-- | Bitwise /or/ of two bit masks.
(.+.) :: Int -> Int -> Int
(.+.) i j
  = i .|. j

-- | Unset certain bits in a bitmask.
(.-.) :: Int -> BitFlag -> Int
(.-.) i j
  = i .&. complement j

-- | Bitwise /or/ of a list of bit masks.
bits :: [Int] -> Int
bits xs
  = foldr (.+.) 0 xs

-- | (@bitsSet mask i@) tests if all bits in @mask@ are also set in @i@.
bitsSet :: Int -> Int -> Bool
bitsSet mask i
  = (i .&. mask == mask)


{--------------------------------------------------------------------------------
  Id
--------------------------------------------------------------------------------}
{-# NOINLINE varTopId #-}
varTopId :: Var Id
varTopId
  = unsafePerformIO (varCreate (wxID_HIGHEST+1))

-- | When creating a new window you may specify 'idAny' to let wxWindows
-- assign an unused identifier to it automatically. Furthermore, it can be
-- used in an event connection to handle events for any identifier.
idAny :: Id
idAny
  = -1

-- | Create a new unique identifier.
idCreate :: IO Id
idCreate
  = varUpdate varTopId (+1)



{--------------------------------------------------------------------------------
  Control
--------------------------------------------------------------------------------}
-- | Ignore the result of an 'IO' action.
unitIO :: IO a -> IO ()
unitIO io
  = do io; return ()

-- | Perform an action when a test succeeds.
when :: Bool -> IO () -> IO ()
when = M.when

-- | Properly release resources, even in the event of an exception.
bracket :: IO a           -- ^ computation to run first (acquire resource)
           -> (a -> IO b) -- ^ computation to run last (release resource)
           -> (a -> IO c) -- ^ computation to run in-between (use resource)
           -> IO c
bracket = CE.bracket

-- | Specialized variant of 'bracket' where the return value is not required.
bracket_ :: IO a     -- ^ computation to run first (acquire resource)
           -> IO b   -- ^ computation to run last (release resource)
           -> IO c   -- ^ computation to run in-between (use resource)
           -> IO c
bracket_ = CE.bracket_

-- | Run some computation afterwards, even if an exception occurs.
finally :: IO a -- ^ computation to run first
        -> IO b -- ^ computation to run last (release resource)
        -> IO a
finally = CE.finally

-- | Run some computation afterwards, even if an exception occurs. Equals 'finally' but
-- with the arguments swapped.
finalize ::  IO b -- ^ computation to run last (release resource)
          -> IO a -- ^ computation to run first
          -> IO a
finalize last first
  = finally first last

{--------------------------------------------------------------------------------
  Variables
--------------------------------------------------------------------------------}

-- | A mutable variable. Use this instead of 'MVar's or 'IORef's to accomodate for
-- future expansions with possible concurrency.
type Var a  = IORef a

-- | Create a fresh mutable variable.
varCreate :: a -> IO (Var a)
varCreate x    = newIORef x

-- | Get the value of a mutable variable.
varGet :: Var a -> IO a
varGet v    = readIORef v

-- | Set the value of a mutable variable.
varSet :: Var a -> a -> IO ()
varSet v x = writeIORef v x

-- | Swap the value of a mutable variable.
varSwap :: Var a -> a -> IO a
varSwap v x = do prev <- varGet v; varSet v x; return prev

-- | Update the value of a mutable variable and return the old value.
varUpdate :: Var a -> (a -> a) -> IO a
varUpdate v f = do x <- varGet v
                   varSet v (f x)
                   return x



{-----------------------------------------------------------------------------------------
  Point
-----------------------------------------------------------------------------------------}
pointMove :: Vector -> Point -> Point
pointMove (Vector dx dy) (Point x y)
  = Point (x+dx) (y+dy)

pointMoveBySize :: Point -> Size -> Point
pointMoveBySize (Point x y) (Size w h)  = Point (x + w) (y + h)

pointAdd :: Point -> Point -> Point
pointAdd (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

pointSub :: Point -> Point -> Point
pointSub (Point x1 y1) (Point x2 y2) = Point (x1-x2) (y1-y2)

pointScale :: Point -> Int -> Point
pointScale (Point x y) v = Point (v*x) (v*y)


{-----------------------------------------------------------------------------------------
  Size
-----------------------------------------------------------------------------------------}
-- | Return the width. (see also 'sizeW').
sizeWidth :: Size -> Int
sizeWidth (Size w h)
  = w

-- | Return the height. (see also 'sizeH').
sizeHeight :: Size -> Int
sizeHeight (Size w h)
  = h

-- | Returns 'True' if the first size totally encloses the second argument.
sizeEncloses :: Size -> Size -> Bool
sizeEncloses (Size w0 h0) (Size w1 h1)
  = (w0 >= w1) && (h0 >= h1)

-- | The minimum of two sizes.
sizeMin :: Size -> Size -> Size
sizeMin (Size w0 h0) (Size w1 h1)
  = Size (min w0 w1) (min h0 h1)

-- | The maximum of two sizes.
sizeMax :: Size -> Size -> Size
sizeMax (Size w0 h0) (Size w1 h1)
  = Size (max w0 w1) (max h0 h1)

{-----------------------------------------------------------------------------------------
  Vector
-----------------------------------------------------------------------------------------}
vecNegate :: Vector -> Vector
vecNegate (Vector x y)
  = Vector (-x) (-y)

vecOrtogonal :: Vector -> Vector
vecOrtogonal (Vector x y) = (Vector y (-x))

vecAdd :: Vector -> Vector -> Vector
vecAdd (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)

vecSub :: Vector -> Vector -> Vector
vecSub (Vector x1 y1) (Vector x2 y2) = Vector (x1-x2) (y1-y2)

vecScale :: Vector -> Int -> Vector
vecScale (Vector x y) v = Vector (v*x) (v*y)

vecBetween :: Point -> Point -> Vector
vecBetween (Point x1 y1) (Point x2 y2) = Vector (x2-x1) (y2-y1)

vecLength :: Vector -> Double
vecLength (Vector x y)
  = sqrt (fromIntegral (x*x + y*y))

{-----------------------------------------------------------------------------------------
  Rectangle
-----------------------------------------------------------------------------------------}
rectContains :: Rect -> Point -> Bool
rectContains (Rect l t w h) (Point x y) 
  = (x >= l && x <= (l+w) && y >= t && y <= (t+h))

rectMoveTo :: Rect -> Point -> Rect
rectMoveTo r p
  = rect p (rectSize r)

rectFromPoint :: Point -> Rect
rectFromPoint (Point x y)
  = Rect x y x y

rectCentralPoint :: Rect -> Point
rectCentralPoint (Rect l t w h)
  = Point (l + div w 2) (t + div h 2)

rectCentralRect :: Rect -> Size -> Rect
rectCentralRect r@(Rect l t rw rh) (Size w h)
  = let c = rectCentralPoint r
    in Rect (pointX c - (w - div w 2)) (pointY c - (h - div h 2)) w h


rectStretchTo :: Rect -> Size -> Rect
rectStretchTo (Rect l t _ _) (Size w h)
  = Rect l t w h

rectMove :: Rect -> Vector -> Rect
rectMove  (Rect x y w h) (Vector dx dy)
  = Rect (x+dx) (y+dy) w h

rectOverlaps :: Rect -> Rect -> Bool
rectOverlaps (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2)
  = (x1+w1 >= x2 && x1 <= x2+w2) && (y1+h1 >= y2 && y1 <= y2+h2)


-- | A list with rectangles that constitute the difference between two rectangles.
rectsDiff :: Rect -> Rect -> [Rect]
rectsDiff rect1 rect2
  = subtractFittingRect rect1 (rectOverlap rect1 rect2)
  where
    -- subtractFittingRect r1 r2 subtracts r2 from r1 assuming that r2 fits inside r1
    subtractFittingRect :: Rect -> Rect -> [Rect]
    subtractFittingRect r1 r2 =
            filter (not . rectIsEmpty)
                    [ rectBetween (rectTopLeft r1) (rectTopRight r2)
                    , rectBetween (pt (rectLeft r1) (rectTop r2)) (rectBottomLeft r2)
                    , rectBetween (pt (rectLeft r1) (rectBottom r2)) (pt (rectRight r2) (rectBottom r1))
                    , rectBetween (rectTopRight r2) (rectBottomRight r1)
                    ]

rectUnion :: Rect -> Rect -> Rect
rectUnion r1 r2
  = rectBetween (pt (min (rectLeft r1) (rectLeft r2)) (min (rectTop r1) (rectTop r2)))
         (pt (max (rectRight r1) (rectRight r2)) (max (rectBottom r1) (rectBottom r2)))

rectUnions :: [Rect] -> Rect
rectUnions []
  = rectZero
rectUnions (r:rs)
  = foldr rectUnion r rs

-- | The intersection between two rectangles.
rectOverlap :: Rect -> Rect -> Rect
rectOverlap r1 r2
  | rectOverlaps r1 r2  = rectBetween (pt (max (rectLeft r1) (rectLeft r2)) (max (rectTop r1) (rectTop r2)))
                               (pt (min (rectRight r1) (rectRight r2)) (min (rectBottom r1) (rectBottom r2)))
  | otherwise           = rectZero


{-----------------------------------------------------------------------------------------
 Default colors.
-----------------------------------------------------------------------------------------}
black, darkgrey, dimgrey, mediumgrey, grey, lightgrey, white :: Color
red, green, blue :: Color
cyan, magenta, yellow :: Color

black     = colorRGB 0x00 0x00 0x00
darkgrey  = colorRGB 0x2F 0x2F 0x2F
dimgrey   = colorRGB 0x54 0x54 0x54
mediumgrey= colorRGB 0x64 0x64 0x64
grey      = colorRGB 0x80 0x80 0x80
lightgrey = colorRGB 0xC0 0xC0 0xC0
white     = colorRGB 0xFF 0xFF 0xFF

red       = colorRGB 0xFF 0x00 0x00
green     = colorRGB 0x00 0xFF 0x00
blue      = colorRGB 0x00 0x00 0xFF

yellow    = colorRGB 0xFF 0xFF 0x00
magenta   = colorRGB 0xFF 0x00 0xFF
cyan      = colorRGB 0x00 0xFF 0xFF