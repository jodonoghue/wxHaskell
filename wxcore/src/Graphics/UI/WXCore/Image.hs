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
      -- * Helpers
    , imageTypeFromExtension
    , imageTypeFromFileName
    ) where

import Char( toLower )
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types


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