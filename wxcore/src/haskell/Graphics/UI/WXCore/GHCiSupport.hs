{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Graphics.UI.WXCore.GHCiSupport(enableGUI) where
-- GHCi support on MacOS X
-- TODO: preprocessor to make it conditional on the platform

#if darwin_HOST_OS

import Data.Int
import Foreign

type ProcessSerialNumber = Int64

foreign import ccall "GetCurrentProcess" getCurrentProcess :: Ptr ProcessSerialNumber -> IO Int16
foreign import ccall "_CGSDefaultConnection" cgsDefaultConnection :: IO ()
foreign import ccall "CPSEnableForegroundOperation" cpsEnableForegroundOperation :: Ptr ProcessSerialNumber -> IO ()
foreign import ccall "CPSSignalAppReady" cpsSignalAppReady :: Ptr ProcessSerialNumber -> IO ()
foreign import ccall "CPSSetFrontProcess" cpsSetFrontProcess :: Ptr ProcessSerialNumber -> IO ()

enableGUI :: IO ()
enableGUI = alloca $ \psn -> do
    getCurrentProcess psn
    cgsDefaultConnection
    cpsEnableForegroundOperation psn
    cpsSignalAppReady psn
    cpsSetFrontProcess psn

#else

enableGUI :: IO ()
enableGUI = return ()

#endif