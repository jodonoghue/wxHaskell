module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcTypes

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array

main 
  = console

gui
  = start $ 
    do f   <- frame [text := "Pubs database"]

       -- use text control as logger
       log <- textCtrl f WrapLine [enable := False] 
       textCtrlMakeLogActiveTarget log
       
       -- layout
       set f [layout     := fill (widget log)
             ,clientSize := sz 300 200
             ]
       
       -- test
       dir <- getApplicationDir
       logMessage ("application dir: " ++ dir)

       srcs <- dbGetDataSources
       mapM_ (\(name,desc) -> logMessage ("source: " ++ name ++ ": " ++ desc)) srcs

console :: IO ()
console
  = do srcs <- dbGetDataSources
       putStrLn $ unlines $ ["source: " ++ name ++ ": " ++ desc | (name,desc) <- srcs]

       putStrLn "opening pubs datasource"
       connectInf <- dbConnectInfCreate nullHENV "pubs" "" "" "" "" "" 
       db         <- dbGetConnection connectInf True
       if (objectIsNull db)
         then putStrLn "unable to open database!"
         else do ok <- dbExecSql db "SELECT au_fname FROM authors"
                 if not ok
                   then putStrLn "unable to exec query"
                   else do names <- dbGetNames db
                           putStrLn $ unlines $ names                          
                           dbFreeConnection db
                           dbConnectInfDelete connectInf
                           dbCloseConnections 
                           putStrLn "done"

dbGetNames :: Db a -> IO [String]
dbGetNames db
  = do ok <- dbGetNext db
       if not ok
        then return []
        else do name  <- dbGetDataStringEx db 1024 1
                names <- dbGetNames db
                return (name:names)

dbGetDataStringEx :: Db a -> Int -> Int -> IO String
dbGetDataStringEx db maxLen column
  = allocaArray maxLen $ \cstr ->
    do ok <- dbGetDataString db column cstr maxLen nullPtr
       if not ok
        then ioError (userError "database: unable to retrieve string value.")
        else peekCString cstr 

-- | Returns the name and description of the data sources on the system.
dbGetDataSources :: IO [(String,String)]
dbGetDataSources 
  = do connectInf <- dbConnectInfCreate nullHENV "" "" "" "" "" ""
       henv       <- dbConnectInfGetHenv connectInf
       xs         <- loop henv True
       dbConnectInfDelete connectInf
       return xs
  where
    loop henv isFirst
      = do mbSrc <- dbGetDataSourceEx henv isFirst
           case mbSrc of
             Nothing  -> return [("test","test")]
             Just x   -> do xs <- loop henv False
                            return (x:xs)

dbGetDataSourceEx :: HENV () -> Bool -> IO (Maybe (String,String))
dbGetDataSourceEx henv isFirst
  = allocaArray (dsnLen+1)  $ \cdsn  ->
    allocaArray (descLen+1) $ \cdesc ->
    do pokeArray0 0 cdsn []
       pokeArray0 0 cdesc []
       ok   <- dbGetDataSource henv (castPtr cdsn) dsnLen (castPtr cdesc) descLen (if isFirst then wxSQL_FETCH_FIRST else wxSQL_FETCH_NEXT)
       if not ok
        then return Nothing
        else do dsn  <- peekCString cdsn
                desc <- peekCString cdesc 
                return (Just (dsn,desc))
  where
    dsnLen  = 255
    descLen = 1024
