-----------------------------------------------------------------------------------------
{-| Module      :  Process
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Process and stream wrappers.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXH.Process
        (
        -- * Process
          OnReceive, OnEndProcess
        , processExecAsync
        -- * Streams
        , outputStreamPutString
        , inputStreamReadContents
        , inputStreamReadContentsN
        , inputStreamReadString
        ) where

import System.IO.Unsafe( unsafeInterleaveIO )
import Graphics.UI.WXH.WxcTypes( ptrCast )
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.WxcClasses
import Graphics.UI.WXH.Types
import Graphics.UI.WXH.Events

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array


-- | Write a string to an output stream.
outputStreamPutString :: OutputStream a -> String -> IO ()
outputStreamPutString outputStream s
  = withCString s $ \cstr ->
    outputStreamWrite outputStream cstr (length s)

-- | Get the entire contents of an input stream. The content
-- is returned as a lazy stream (like 'hGetContents').
inputStreamReadContents :: InputStream a -> IO String
inputStreamReadContents inputStream
  = inputStreamReadContentsN inputStream 1

-- | Get the entire contents of an input stream. The content
-- is returned as a lazy stream (like 'hGetContents'). The
-- contents are returned in lazy /batches/, whose size is
-- determined by the first parameter.
inputStreamReadContentsN :: InputStream a -> Int -> IO String
inputStreamReadContentsN inputStream n
  = do status <- streamBaseGetLastError inputStream
       if (status == wxSTREAM_NO_ERROR)
        then do x  <- inputStreamReadString inputStream n
                xs <- unsafeInterleaveIO (inputStreamReadContentsN inputStream n)
                return (x ++ xs)
        else return ""

-- | The expression (@inputStreamReadString n input@) reads a string of maximally
-- @n@ characters from @input@.
inputStreamReadString :: InputStream a -> Int -> IO String
inputStreamReadString inputStream n   | n <= 1
  = do c <- inputStreamGetC inputStream
       nread <- inputStreamLastRead inputStream
       if (nread <= 0)
        then return ""
        else return [c]

inputStreamReadString inputStream n
  = allocaBytes (n+1) $ \buffer ->
    do inputStreamRead inputStream buffer n
       nread <- inputStreamLastRead inputStream
       if (nread > 0)
        then peekArray nread buffer
        else return ""

-- | Type of input receiver function.
type OnReceive     = String -> StreamStatus -> IO ()

-- | Type of end-of-process event handler. Gets the exitcode as its argument.
type OnEndProcess  = Int -> IO ()

-- | (@processExecAsync command bufferSize onEndProcess onOutput onErrorOutput parent@) starts
-- the @command@ asynchronously. The handler @onEndProcess@ is called when the process
-- terminates. @onOutput@ receives the output from @stdout@, while @onErrorOutput@ receives
-- output from @stderr@. The @bufferSize@ determines the intermediate buffer used to
-- cache the output from those channels. The calls returns a triple (@send,process,pid@):
-- The @send@ function is used to send input to the @stdin@ pipe of the process. The
-- process object is returned in @process@ and the process identifier in @pid@.
processExecAsync :: Window a -> String -> Int -> OnEndProcess -> OnReceive -> OnReceive
                      -> IO (String -> IO (), Process (), Int)
processExecAsync parent command bufferSize onEndProcess onOutput onErrOutput
  = do process    <- processCreateDefault parent idAny
       processRedirect process
       pid        <- wxcAppExecuteProcess command wxEXEC_ASYNC process
       if (pid == 0)
        then return (\s -> return (), objectNull, pid)
        else do inputPipe  <- processGetInputStream process
                outputPipe <- processGetOutputStream process
                errorPipe  <- processGetErrorStream process
                evtHandlerOnEndProcess parent (handleOnEndProcess pid process inputPipe outputPipe errorPipe)
                evtHandlerOnInput parent onOutput inputPipe bufferSize
                evtHandlerOnInput parent onErrOutput errorPipe bufferSize
                let send txt   = outputStreamPutString outputPipe txt
                return (send, process, pid)
  where
    handleOnEndProcess ourPid process inputPipe outputPipe errorPipe pid exitcode
      | ourPid == pid  = do onEndProcess exitcode
                            processDelete process
      | otherwise      = return ()