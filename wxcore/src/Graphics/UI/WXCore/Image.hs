--------------------------------------------------------------------------------
{-| Module      :  Image
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable
-}
--------------------------------------------------------------------------------
module Graphics.UI.WXCore.Image
    ( -- * Images
      frameSetIconFromFile
    -- ** Imagelist
    , imageListAddIconsFromFiles
    , imageListAddIconFromFile
    -- ** Icons
    , withIconFromFile
    , iconCreateFromFile
    , iconGetSize
    -- ** Bitmaps
    , withBitmapFromFile
    , bitmapCreateFromFile
    , bitmapGetSize
    , bitmapSetSize
    -- ** Helpers
    , imageTypeFromExtension
    , imageTypeFromFileName

    -- * Direct image manipulation
    , imageCreateFromPixelBuffer
    , imageGetPixelBuffer

    -- ** Pixel buffer
    , PixelBuffer
    , pixelBufferCreate
    , pixelBufferDelete
    , pixelBufferInit
    , pixelBufferSetPixel
    , pixelBufferGetPixel    
    ) where

import Char( toLower )
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Defines


{-----------------------------------------------------------------------------------------
  ImageList
-----------------------------------------------------------------------------------------}
-- | Initialize an image list with icons from files. Use a 'sizeNull' to
-- use the native size of the loaded icons.
imageListAddIconsFromFiles :: ImageList a -> Size -> [FilePath] -> IO ()
imageListAddIconsFromFiles images desiredSize fnames
  = mapM_ (imageListAddIconFromFile images desiredSize) fnames

-- | Add an icon from a file to an imagelist.
imageListAddIconFromFile :: ImageList a -> Size -> FilePath -> IO ()
imageListAddIconFromFile images desiredSize fname
  = do image <- imageCreateFromFile fname
       imageRescale image desiredSize
       bitmap <- imageConvertToBitmap image
       imageListAddBitmap images bitmap nullBitmap
       bitmapDelete bitmap
       imageDelete image
       return ()

{-----------------------------------------------------------------------------------------
  Icons
-----------------------------------------------------------------------------------------}

-- | Set the icon of a frame.
frameSetIconFromFile :: Frame a -> FilePath -> IO ()
frameSetIconFromFile f fname
  = withIconFromFile fname sizeNull (frameSetIcon f)


-- | Load an icon (see 'iconCreateFromFile') and automatically delete it
-- after use.
withIconFromFile :: FilePath -> Size -> (Icon () -> IO a) -> IO a
withIconFromFile fname size f
  = bracket (iconCreateFromFile fname size)
            (iconDelete)
            f

-- | Load an icon from an icon file (ico,xbm,xpm,gif). The 'Size' argument
-- gives the desired size but can be 'sizeNull' to retrieve the image
-- in its natural size. 
iconCreateFromFile :: FilePath -> Size -> IO (Icon ())
iconCreateFromFile fname size
  = iconCreateLoad fname (imageTypeFromFileName fname) size
  {-
    do icon <- iconCreateLoad fname (imageTypeFromFileName fname) size
       ok   <- iconOk icon
       if (ok)
        then return icon
        else do iconDelete icon
                ioError (userError ("unable to load icon: " ++ show fname))
  -}

-- | Get the size of an icon.
iconGetSize :: Icon a -> IO Size
iconGetSize icon
  = do w <- iconGetWidth icon
       h <- iconGetHeight icon
       return (sz w h)

{-----------------------------------------------------------------------------------------
  Bitmaps
-----------------------------------------------------------------------------------------}

-- | Load a bitmap (see 'bitmapCreateFromFile') and automatically delete it
-- after use.
withBitmapFromFile :: FilePath -> (Bitmap () -> IO a) -> IO a
withBitmapFromFile fname f
  = bracket (bitmapCreateFromFile fname)
            (bitmapDelete)
            f

-- | Load a bitmap from an image file (gif, jpg, png, etc.) 
bitmapCreateFromFile :: FilePath -> IO (Bitmap ())
bitmapCreateFromFile fname
  = bitmapCreateLoad fname (imageTypeFromFileName fname)
    {-
    do bm <- bitmapCreateLoad fname (imageTypeFromFileName fname)
       ok <- bitmapOk bm
       if (ok)
        then return bm
        else do bitmapDelete bm
                ioError (userError ("unable to load image: " ++ show fname))
    -}

-- | The size of a bitmap.
bitmapGetSize :: Bitmap a -> IO Size
bitmapGetSize bitmap
  = do w <- bitmapGetWidth bitmap
       h <- bitmapGetHeight bitmap
       return (sz w h)

-- | Set the size of a bitmap.
bitmapSetSize :: Bitmap a -> Size -> IO ()
bitmapSetSize bitmap (Size w h)
  = do bitmapSetWidth bitmap w
       bitmapSetHeight bitmap h


{-----------------------------------------------------------------------------------------
  Images
-----------------------------------------------------------------------------------------}

-- | Get an image type from a file name.
imageTypeFromFileName :: String -> BitFlag
imageTypeFromFileName fname
  = imageTypeFromExtension (map toLower (reverse (takeWhile (/= '.') (reverse fname))))

-- | Get an image type from a file extension.
imageTypeFromExtension :: String -> BitFlag
imageTypeFromExtension ext
  = case ext of
      "jpg"   -> wxBITMAP_TYPE_JPEG
      "jpeg"  -> wxBITMAP_TYPE_JPEG
      "gif"   -> wxBITMAP_TYPE_GIF
      "bmp"   -> wxBITMAP_TYPE_BMP
      "png"   -> wxBITMAP_TYPE_PNG
      "xpm"   -> wxBITMAP_TYPE_XPM
      "xbm"   -> wxBITMAP_TYPE_XBM
      "pcx"   -> wxBITMAP_TYPE_PCX
      "ico"   -> wxBITMAP_TYPE_ICO
      "tif"   -> wxBITMAP_TYPE_TIF
      "tiff"  -> wxBITMAP_TYPE_TIF
      "pnm"   -> wxBITMAP_TYPE_PNM
      "pict"  -> wxBITMAP_TYPE_PICT
      "icon"  -> wxBITMAP_TYPE_ICON
      other   -> wxBITMAP_TYPE_ANY

{-----------------------------------------------------------------------------------------
  Direct image manipulation
-----------------------------------------------------------------------------------------}
-- | An abstract pixel buffer (= array of RGB values)
data PixelBuffer   = PixelBuffer Bool Size (Ptr Char)

-- | Create a pixel buffer. (To be deleted with 'pixelBufferDelete').
pixelBufferCreate   :: Size -> IO PixelBuffer
pixelBufferCreate size
  = do buffer <- wxcMalloc (sizeW size * sizeH size * 3)
       return (PixelBuffer True size (ptrCast buffer))

-- | Delete a pixel buffer.
pixelBufferDelete  :: PixelBuffer -> IO ()
pixelBufferDelete (PixelBuffer owned size buffer)
  = when (owned && not (ptrIsNull buffer)) (wxcFree buffer)

-- | Initialize the pixel buffer with a grey color. The second argument
-- specifies the /greyness/ as a number between 0.0 (black) and 1.0 (white).
pixelBufferInit :: PixelBuffer -> Color -> IO ()
pixelBufferInit (PixelBuffer owned size buffer) color
  = wxcInitPixelsRGB buffer size (intFromColor color)

-- | Set the color of a pixel.
pixelBufferSetPixel :: PixelBuffer -> Point -> Color -> IO ()
pixelBufferSetPixel (PixelBuffer owned size buffer) point color
  = wxcSetPixelRGB buffer (sizeW size) point (intFromColor color)

-- | Get the color of a pixel
pixelBufferGetPixel :: PixelBuffer -> Point -> IO Color
pixelBufferGetPixel (PixelBuffer owned size buffer) point
  = do rgb <- wxcGetPixelRGB buffer (sizeW size) point
       return (colorFromInt rgb)
  
-- | Create an image from a pixel buffer.
imageCreateFromPixelBuffer :: PixelBuffer -> IO (Image ())
imageCreateFromPixelBuffer (PixelBuffer owned size buffer) 
  = imageCreateFromDataEx size buffer True

-- | Get the pixel buffer of an image.
imageGetPixelBuffer :: Image a -> IO PixelBuffer
imageGetPixelBuffer image
  = do ptr <- imageGetData image
       w   <- imageGetWidth image
       h   <- imageGetHeight image
       return (PixelBuffer False (sz w h) (ptrCast ptr))