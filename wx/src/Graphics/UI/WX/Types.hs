{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Types
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

Basic types.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Types
    (

    -- * Types
    -- ** Objects
      ( # )
    , Object, objectNull, objectIsNull, objectCast
    , Managed, managedNull, managedIsNull, managedCast, createManaged, withManaged, managedTouch

    -- ** Identifiers
    , Id, idAny, idCreate

    -- ** Misc.
    , Style
    , EventId

    -- * Control
    , unitIO, bracket, bracket_, finally, finalize, when

    -- * Variables
    , Var, varCreate, varGet, varSet, varUpdate, varSwap

    -- * Bits
    , BitMask(..), mask, (.+.), (.-.), bits, bitsSet

    -- * Basic types

    -- ** Booleans
    , boolFromInt, intFromBool

    -- ** Colors
    , Color, colorRGB, colorRed, colorGreen, colorBlue
    , black, darkgrey, dimgrey, mediumgrey, grey, lightgrey, white
    , red, green, blue
    , cyan, magenta, yellow

    -- ** Points
    , Point(..), pt, pointFromVec, pointFromSize, pointZero, pointNull
    , pointMove, pointMoveBySize, pointAdd, pointSub, pointScale

    -- ** Sizes
    , Size(..), sz, sizeFromPoint, sizeFromVec, sizeZero, sizeNull, sizeEncloses

    -- ** Vectors
    , Vector(..), vec, vecFromPoint, vecFromSize, vecZero, vecNull
    , vecNegate, vecOrtogonal, vecAdd, vecSub, vecScale, vecDistance

    -- ** Rectangles
    , Rect(..)
    , topLeft, topRight, bottomLeft, bottomRight, bottom, right
    , rect, rectBetween, rectFromSize, rectZero, rectNull, rectSize, rectIsEmpty
    , rectContains, rectMoveTo, rectFromPoint, rectCentralPoint, rectCentralRect, rectStretchTo
    , rectMove, rectOverlaps, rectsDiff, rectUnion, rectOverlap, rectUnions
    ) where

import Graphics.UI.WXH.Types

-- | Data types that can be represented through a bit mask. Only the @assocBitMask@ method
-- is required for a new instance.
class Eq b => BitMask b where
  -- | Give the association between the constructors and the bits. If a constructor
  -- corresponds to no bits set, it should come as the last element.
  assocBitMask :: [(b,Int)]

  -- | Convert to a bitmask
  toBitMask    :: b -> Int
  -- | Convert from a bitmask
  fromBitMask  :: Int -> b
  -- | Set the correct bits corresponding to a constructor in a mask.
  setBitMask :: b -> Int -> Int

  toBitMask x
    = case lookup x assocBitMask of
        Just m  -> m
        Nothing -> 0

  fromBitMask i
    = walk assocBitMask
    where
      walk []         = error "Graphics.UI.WX.Types.fromBitMask: empty list"
      walk [(x,0)]    = x
      walk ((x,m):xs) | bitsSet m i = x
                      | otherwise   = walk xs

  setBitMask x i
    = i .-. (bits (map snd (assocBitMask::[(b,Int)]))) .+. toBitMask x


-- | Create a bitmask from a list of types.
mask :: BitMask b => [b] -> Int
mask xs
  = foldr (.+.) 0 (map toBitMask xs)