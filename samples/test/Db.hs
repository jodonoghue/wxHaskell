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
       putStrLn "datasources:"
       putStrLn $ unlines $ [" " ++ adjust 25 name ++ ": " ++ desc | (name,desc) <- srcs]

showPubsNames :: IO ()
showPubsNames 
  = do putStrLn "opening pubs datasource"
       db <- dbConnect "pubs" "" ""
       if (objectIsNull db)
         then putStrLn "unable to open database!"
         else do ok <- dbExecSql db "SELECT au_fname FROM authors"
                 if not ok
                   then putStrLn "unable to exec query"
                   else do names <- dbGetNames db
                           putStrLn $ unlines $ names                          
                           dbFreeConnection db
                           -- dbConnectInfDelete connectInf
                           dbCloseConnections 
                           putStrLn "done"


printInfo :: String -> IO ()
printInfo dbName
  = do db   <- dbConnect dbName "" ""
       info <- dbGetInfo db
       dbFreeConnection db
       printDbInfo info

printDbInfo info
  = putStr $ unlines $ showDbInfo info
    



{----------------------------------------------------------

----------------------------------------------------------}

dbGetNames :: Db a -> IO [String]
dbGetNames db
  = do ok <- dbGetNext db
       if not ok
        then return []
        else do name  <- dbGetDataStringEx db 1024 1
                names <- dbGetNames db
                return (name:names)


-- | Get a string field from a column. Takes the maximal string length
-- and a column number as arguments.
dbGetDataStringEx :: Db a -> Int -> Int -> IO String
dbGetDataStringEx db maxLen column
  = allocaArray maxLen $ \cstr ->
    do ok <- dbGetDataString db column cstr maxLen nullPtr
       if not ok
        then ioError (userError "database: unable to retrieve string value.")
        else peekCString cstr 


{----------------------------------------------------------

----------------------------------------------------------}
-- | Database information.
data DbInfo
  = DbInfo   { dbCatalog  :: String       -- ^ System name of the database
             , dbSchema   :: String       -- ^ Schema name
             , dbTables   :: [TableInfo]  -- ^ The tables of the database
             }

-- | Database table information.
data TableInfo
  = TableInfo{ tableName    :: String     -- ^ Name of the table.
             , tableType    :: String     -- ^ Type of the table (ie. @SYSTEM TABLE@, @TABLE@, etc)
             , tableRemarks :: String     -- ^ Comments
             , tableColumns :: [ColumnInfo] -- ^ The columns of the table.
             }

-- | Database column information.
data ColumnInfo
  = ColumnInfo{ columnName     :: String  -- ^ Column name.
              , columnSize     :: Int     -- ^ Length of the column.
              , columnNullable :: Bool    -- ^ Are NULL values allowed?  
              , columnType     :: Int     -- ^ Logical type
              , columnSqlType  :: Int     -- ^ SQL type
              , columnTypeName :: String  -- ^ Logical type name (ie. @VARCHAR@, @INTEGER@ etc.)
              , columnRemarks  :: String  -- ^ Comments

              , columnDecimalDigits    :: Int     -- ^ Number of decimal digits (for currency for example).
              , columnNumPrecRadix     :: Int     -- ^ Radix precision
              , columnIsForeignKey     :: Bool    -- ^ Is this a foreign key column?
              , columnIsPrimaryKey     :: Bool    -- ^ Is this a primary key column?
              , columnForeignTableName :: String  -- ^ Tables that use this foreign key.
              , columnPrimaryTableName :: String  -- ^ Tables that use this primary key.
              }


showDbInfo :: DbInfo -> [String]
showDbInfo info
  = ["catalog: " ++ dbCatalog info
    ,"schema : " ++ dbSchema info
    ,"tables : "
    ] ++ 
    numbered (map showTableInfo (dbTables info))


showTableInfo :: TableInfo -> [String]
showTableInfo info
  = ["name   : " ++ tableName info
    ,"type   : " ++ tableType info
    ,"remarks: " ++ tableRemarks info
    ,"columns: "
    ] ++
    numbered (map showColumnInfo (tableColumns info))


numbered xss
  = concat [shift (" " ++ adjust 3 (show i ++ ":")) xs | (i,xs) <- zip [1..] xss]
  where
    shift prefix []
      = []
    shift prefix (x:xs)
      = [prefix ++ x] ++ map (replicate (length prefix) ' ' ++) xs

adjust :: Int -> String -> String
adjust n s  | length s < n  = s ++ replicate (n - length s) ' '
            | otherwise     = s



showColumnInfo info
  = ["name   : " ++ columnName info
    ,"type   : " ++ columnTypeName info
    ,"size   : " ++ show (columnSize info)
    ,"sqltp  : " ++ show (columnSqlType info)
    ,"type id: " ++ show (columnType info)
    ,"digits : " ++ show (columnDecimalDigits info)
    ,"prec   : " ++ show (columnNumPrecRadix info)
    ,"remarks: " ++ columnRemarks info
    ,"pkey   : " ++ show (columnIsPrimaryKey info)
    ,"ptable : " ++ columnPrimaryTableName info
    ,"fkey   : " ++ show (columnIsForeignKey info)
    ,"ftable : " ++ columnForeignTableName info
    ]


-- | Get the complete meta information of a database.
dbGetInfo :: Db a -> IO DbInfo
dbGetInfo db
  = do dbInf    <- dbGetCatalog db ""
       catalog  <- dbInfGetCatalogName dbInf
       schema   <- dbInfGetSchemaName  dbInf
       numTables<- dbInfGetNumTables   dbInf
       tables   <- mapM (\idx -> do tableInf <- dbInfGetTableInf dbInf (idx-1)
                                    dbTableInfGetInfo tableInf db) 
                        [1..numTables]
       return (DbInfo catalog schema tables)

dbTableInfGetInfo :: DbTableInf a -> Db b -> IO TableInfo
dbTableInfGetInfo tableInf db
  = do tableName <- dbTableInfGetTableName tableInf
       tableType <- dbTableInfGetTableType tableInf
       remarks   <- dbTableInfGetTableRemarks tableInf
       numCols   <- dbTableInfGetNumCols tableInf
       columns   <- dbGetColumnInfos db tableName  
       return (TableInfo tableName tableType remarks columns)

-- | Return the column information of a certain table.
dbGetColumnInfos :: Db a -> String -> IO [ColumnInfo]
dbGetColumnInfos db tableName
  = alloca $ \pcnumCols ->
    bracket (dbGetColumns db tableName pcnumCols "")
            (dbColInfArrayDelete)
            (\colInfs  -> do cnumCols <- peek pcnumCols
                             let numCols = fromCInt cnumCols
                             mapM (\idx -> do colInf <- dbColInfArrayGetColInf colInfs (idx-1)
                                              dbColInfGetInfo colInf) 
                                  [1..numCols])
        
             
dbColInfGetInfo :: DbColInf a -> IO ColumnInfo
dbColInfGetInfo info
  = do columnName <- dbColInfGetColName info
       columnSize <- dbColInfGetColumnSize info
       nullable   <- dbColInfIsNullable info
       tp         <- dbColInfGetDbDataType info
       sqltp      <- dbColInfGetSqlDataType info
       tpname     <- dbColInfGetTypeName info
       remarks    <- dbColInfGetRemarks info
       decdigits  <- dbColInfGetDecimalDigits info
       numprecrad <- dbColInfGetNumPrecRadix info
       fk         <- dbColInfGetFkCol info
       fkname     <- dbColInfGetFkTableName info
       pk         <- dbColInfGetPkCol info
       pkname     <- dbColInfGetPkTableName info
       return (ColumnInfo columnName columnSize nullable tp sqltp tpname remarks
                          decdigits numprecrad (fk/=0) (pk/=0) fkname pkname )

{----------------------------------------------------------

----------------------------------------------------------}
-- | (@dbConnect name userId password@) creates a connection to a 
-- data source @name@. Returns @NULL@ when the connection failed.
-- The database should still be checked with @dbIsOpen@ to check
-- for initialization errors. Use 'dbFreeConnection' to close the
-- connection.
dbConnect :: String -> String -> String -> IO (Db ())
dbConnect name userId password
  = bracket (dbConnectInfCreate nullHENV name userId password "" "" "" )
            (dbConnectInfDelete)
            (\connectInf -> dbGetConnection connectInf True)
       

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
             Nothing  -> return []
             Just x   -> do xs <- loop henv False
                            return (x:xs)

dbGetDataSourceEx :: HENV () -> Bool -> IO (Maybe (String,String))
dbGetDataSourceEx henv isFirst
  = allocaArray (dsnLen+1)  $ \cdsn  ->
    allocaArray (descLen+1) $ \cdesc ->
    do pokeArray0 0 cdsn []
       pokeArray0 0 cdesc []
       ok   <- dbGetDataSource henv (castPtr cdsn) dsnLen (castPtr cdesc) descLen 
                               (if isFirst then wxSQL_FETCH_FIRST else wxSQL_FETCH_NEXT)
       if not ok
        then return Nothing
        else do dsn  <- peekCString cdsn
                desc <- peekCString cdesc 
                return (Just (dsn,desc))
  where
    dsnLen  = 255
    descLen = 1024
