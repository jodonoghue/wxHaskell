{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Media
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Images, Sounds, and action!
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Media
            ( -- * Sound
              sound, play, playLoop, playWait
              -- * Images
            , bitmap, bitmapCreateFromFile
            ) where

import System.IO.Unsafe( unsafePerformIO )
import Graphics.UI.WXCore 
import Graphics.UI.WX.Types( Var, varGet, varSet, varCreate )
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes

{--------------------------------------------------------------------
  Images
--------------------------------------------------------------------}
-- | Return a managed bitmap object. The file path should point to
-- a valid bitmap file, normally a @.ico@, @.bmp@, @.xpm@, or @.png@,
-- but any file format supported by |Image| is correctly loaded.
--
-- Instances: 'Sized'.
bitmap :: FilePath -> Bitmap ()
bitmap fname
  = unsafePerformIO $ bitmapCreateFromFile fname

instance Sized (Bitmap a) where
  size  = newAttr "size" bitmapGetSize bitmapSetSize

{--------------------------------------------------------------------
  Sounds
--------------------------------------------------------------------}
-- | Return a managed sound object. The file path points to 
-- a valid sound file, normally a @.wav@.
sound :: FilePath -> Wave ()
sound fname 
  = unsafePerformIO $ waveCreate fname False

-- | Play a sound fragment asynchronously.
play :: Wave a -> IO ()
play wave
  = unitIO (wavePlay wave True False)

-- | Play a sound fragment repeatedly (and asynchronously).
playLoop :: Wave a -> IO ()
playLoop wave
  = unitIO (wavePlay wave True True)

-- | Play a sound fragment synchronously (i.e. wait till completion).
playWait :: Wave a -> IO ()
playWait wave
  = unitIO (wavePlay wave False False)

