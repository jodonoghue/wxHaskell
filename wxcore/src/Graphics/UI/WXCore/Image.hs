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
    , pixelBufferInitGrey
    , pixelBufferSetPixel
    , pixelBufferGetPixel    

    -- ** Z-buffer
    , ZBuffer
    , zbufferCreate
    , zbufferDelete
    , zbufferInit
    , zbufferUpdate
    , pixelBufferSetZPixel
    
    ) where

import Char( toLower )
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types


{-----------------------------------------------------------------------------------------
  Icons
-----------------------------------------------------------------------------------------}

-- | Set the icon of a frame.
frameSetIconFromFile :: Frame a -> FilePath -> IO ()
frameSetIconFromFile f fname
  = withIconFromFile fname sizeNull (frameSetIcon f)


-- | Initialize an image list with icons from files.
imageListAddIconsFromFiles :: ImageList a -> [FilePath] -> IO ()
imageListAddIconsFromFiles images fnames
  = mapM_ (imageListAddIconFromFile images) fnames

-- | Add an icon from a file to an imagelist.
imageListAddIconFromFile :: ImageList a -> FilePath -> IO ()
imageListAddIconFromFile images fname
  = do desiredSize <- imageListGetSize images 0
       withIconFromFile fname desiredSize (imageListAddIcon images)
       return ()




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

bufferLength :: Size -> Int
bufferLength size
  = sizeW size * sizeH size * 3

-- | Create a pixel buffer. (To be deleted with 'pixelBufferDelete').
pixelBufferCreate   :: Size -> IO PixelBuffer
pixelBufferCreate size
  = do buffer <- wxcMalloc (bufferLength size)
       return (PixelBuffer True size (ptrCast buffer))

-- | Delete a pixel buffer.
pixelBufferDelete  :: PixelBuffer -> IO ()
pixelBufferDelete (PixelBuffer owned size buffer)
  = when (owned && not (ptrIsNull buffer)) (cFree buffer)

-- | Initialize the pixel buffer with a grey color. The second argument
-- specifies the /greyness/ as a number between 0.0 (black) and 1.0 (white).
pixelBufferInitGrey :: PixelBuffer -> Float -> IO ()
pixelBufferInitGrey (PixelBuffer owned size buffer) greyness
  = wxcPokeBytes buffer 0 (mod (round (greyness * 255.0)) 256) (bufferLength size)

-- | Set the color of a pixel.
pixelBufferSetPixel :: PixelBuffer -> Point -> Color -> IO ()
pixelBufferSetPixel (PixelBuffer owned size buffer) point (Color r g b)
  = wxcSetPixel buffer (sizeW size) point r g b

-- | Get the color of a pixel
pixelBufferGetPixel :: PixelBuffer -> Point -> IO Color
pixelBufferGetPixel (PixelBuffer owned size buffer) point
  = do rgb <- wxcGetPixelRGB buffer (sizeW size) point
       let b = mod rgb 256
           g = mod (div rgb 256) 256
           r = mod (div rgb (256*256)) 256
       return (colorRGB r g b)
  
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


-- | Abstract representation of an 'Int' based Z-buffer (i.e. depth of a pixel).
data ZBuffer      = ZBuffer Size (Ptr Int)

-- | Create a 'ZBuffer' of a certain size.
zbufferCreate :: Size -> IO ZBuffer
zbufferCreate size
  = do buffer <- wxcMallocInts (sizeW size * sizeH size)
       return (ZBuffer size (ptrCast buffer))

-- | Delete a 'ZBuffer'.
zbufferDelete :: ZBuffer -> IO ()
zbufferDelete (ZBuffer size buffer)
  = when (not (ptrIsNull buffer)) (wxcFree buffer)

-- | Initialize a 'ZBuffer' with a certain depth.
zbufferInit :: ZBuffer -> Int -> IO ()
zbufferInit (ZBuffer size buffer) i
  = wxcPokeInts buffer 0 (sizeW size * sizeH size) i

-- | Update a z-value. Returns 'True' when the new z-value is /less/ than
-- the value in the Z-buffer. The Z-buffer is updated with the new z value
-- when this is the case.
zbufferUpdate :: ZBuffer -> Point -> Int -> IO Bool
zbufferUpdate (ZBuffer size buffer) point z
  = wxcUpdateZValue buffer (sizeW size) point z

-- | Set a pixel in a 'PixelBuffer' based on the depth in a 'ZBuffer'. The
-- pixel is only drawn when the depth is /less/ than the current z-value of the
-- pixel. The 'ZBuffer' is updated when the pixel is drawn. This function basically
-- combines 'zbufferUpdate' with 'pixelBufferSetPixel'.
pixelBufferSetZPixel :: PixelBuffer -> ZBuffer -> Point -> Color -> Int -> IO ()
pixelBufferSetZPixel (PixelBuffer owned size buffer) (ZBuffer _ zbuffer) point (Color r g b) z
  = wxcSetZPixel buffer zbuffer (sizeW size) point z r g b 
