module Main where


import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcTypes

import IO( catch, ioError, isUserError, ioeGetErrorString)
import List( isPrefixOf )
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array

main 
  = do printDataSources 
       -- printInfo "pubs"


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

{----------------------------------------------------------
   Print data sources
----------------------------------------------------------}
printDataSources :: IO ()
printDataSources
  = do srcs <- dbGetDataSources
       putStrLn "data sources:"
       putStrLn $ unlines $ [" " ++ adjust 25 name ++ ": " ++ desc | (name,desc) <- srcs]


{----------------------------------------------------------
   Print meta information about  a particular data source
----------------------------------------------------------}
printInfo :: String -> IO ()
printInfo dbName
  = do db   <- dbConnect dbName "" ""
       info <- dbGetInfo db
       dbFreeConnection db
       printDbInfo info

printDbInfo info
  = putStr $ unlines $ showDbInfo info
    

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
  Open connection
----------------------------------------------------------}
-- | Open a connection and automatically close it after the computation
-- returns. See 'dbConnect' for more information about the parameters.
dbWithConnect :: String -> String -> String -> (Db () -> IO b) -> IO b
dbWithConnect name userid password f
  = bracket (dbConnect name userid password)
            (dbDisconnect)
            (f)

-- | (@dbConnect name userId password@) creates a connection to a 
-- data source @name@. Raises a database exception ('DbError') when the connection fails.
-- Use 'dbDisconnect' to close the connection.
dbConnect :: String -> String -> String -> IO (Db ())
dbConnect name userId password
  = bracket (dbConnectInfCreate nullHENV name userId password "" "" "" )
            (dbConnectInfDelete)
            (\connectInf -> do db <- dbGetConnection connectInf True
                               if objectIsNull db 
                                then raiseDbError (DbError "Unable to establish a connection" name DB_ERR_CONNECT 0)
                                else do opened <- dbIsOpen db
                                        if (not opened)
                                         then dbRaiseExn db
                                         else return db)
       

-- | Closes a connection opened with 'dbConnect'.
dbDisconnect :: Db a -> IO ()
dbDisconnect db
  = do dbClose db
       dbHandleExn db $ dbFreeConnection db
       return ()


{----------------------------------------------------------
  Database meta information
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
              , columnType     :: DbType  -- ^ Logical type
              , columnSqlType  :: SqlType -- ^ SQL type
              , columnTypeName :: String  -- ^ Logical type name (ie. @VARCHAR@, @INTEGER@ etc.)
              , columnRemarks  :: String  -- ^ Comments

              , columnDecimalDigits    :: Int     -- ^ Number of decimal digits (for currency for example).
              , columnNumPrecRadix     :: Int     -- ^ Radix precision
              , columnIsForeignKey     :: Bool    -- ^ Is this a foreign key column?
              , columnIsPrimaryKey     :: Bool    -- ^ Is this a primary key column?
              , columnForeignTableName :: String  -- ^ Tables that use this foreign key.
              , columnPrimaryTableName :: String  -- ^ Tables that use this primary key.
              }

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
       return (ColumnInfo columnName columnSize nullable (toEnum tp) (toSqlType sqltp) tpname remarks
                          decdigits numprecrad (fk/=0) (pk/=0) fkname pkname )


{----------------------------------------------------------
  Data sources
----------------------------------------------------------}
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


{----------------------------------------------------------
  Dbms
----------------------------------------------------------}
-- The Database backend system.
data Dbms
  = DbmsORACLE |
    DbmsSYBASE_ASA |        
    DbmsSYBASE_ASE |        
    DbmsMS_SQL_SERVER |
    DbmsMY_SQL |
    DbmsPOSTGRES |
    DbmsACCESS |
    DbmsDBASE |
    DbmsINFORMIX |
    DbmsVIRTUOSO |
    DbmsDB2 |
    DbmsINTERBASE |
    DbmsPERVASIVE_SQL |
    DbmsXBASE_SEQUITER |
    DbmsUNIDENTIFIED 
  deriving (Eq,Enum,Show)

-- | Retrieve the database backend system.
dbGetDbms :: Db a -> IO Dbms
dbGetDbms db
  = do i <- dbDbms db
       if (i==0 || i > fromEnum DbmsUNIDENTIFIED)
        then return DbmsUNIDENTIFIED
        else return (toEnum (i-1))
       

{----------------------------------------------------------
  Database Exceptions
----------------------------------------------------------}
data DbError
  = DbError   { dbErrorMsg   :: String
              , dbDataSource :: String
              , dbErrorCode  :: DbStatus 
              , dbNativeCode :: Int
              }    -- ^ General error.
  | DbNull         -- ^ Reading a @NULL@ value.
  deriving (Read,Show)  


-- | Automatically raise a database exception when 'False' is returned.
dbHandleExn :: Db a -> IO Bool -> IO ()
dbHandleExn db io
  = do ok <- io
       if ok
        then return ()
        else dbRaiseExn db

-- | Raise a database exception based on the errors set in
-- the database.
dbRaiseExn :: Db a -> IO b
dbRaiseExn db
  = do errorMsg  <- dbGetErrorMessage db 0
       errorCode <- dbGetDbStatus db
       nativeCode<- dbGetNativeError db
       dataSource<- dbGetDatasourceName db
       raiseDbError (DbError errorMsg dataSource errorCode nativeCode)

-- | Raise a database error.
raiseDbError :: DbError -> IO a
raiseDbError err
  = ioError (userError (dbErrorPrefix ++ show err))

-- | Handle database errors.
catchDbError :: IO a -> (DbError -> IO a) -> IO a
catchDbError io handler
  = catch io $ \err -> 
    let errmsg = ioeGetErrorString err
    in if (isUserError err && isPrefixOf dbErrorPrefix errmsg)
        then handler (read (drop (length dbErrorPrefix) errmsg))
        else ioError err
     
dbErrorPrefix
  = "Database error: "

{----------------------------------------------------------
  Database Status
----------------------------------------------------------}
-- | Status of the database.
data DbStatus
  = DB_FAILURE                        -- ^ General failure.
  | DB_SUCCESS                        -- ^ No error.
  | DB_ERR_NOT_IN_USE
  | DB_ERR_GENERAL_WARNING                            -- ^ SqlState = @01000@
  | DB_ERR_DISCONNECT_ERROR                           -- ^ SqlState = @01002@
  | DB_ERR_DATA_TRUNCATED                             -- ^ SqlState = @01004@
  | DB_ERR_PRIV_NOT_REVOKED                           -- ^ SqlState = @01006@
  | DB_ERR_INVALID_CONN_STR_ATTR                      -- ^ SqlState = @01S00@
  | DB_ERR_ERROR_IN_ROW                               -- ^ SqlState = @01S01@
  | DB_ERR_OPTION_VALUE_CHANGED                       -- ^ SqlState = @01S02@
  | DB_ERR_NO_ROWS_UPD_OR_DEL                         -- ^ SqlState = @01S03@
  | DB_ERR_MULTI_ROWS_UPD_OR_DEL                      -- ^ SqlState = @01S04@
  | DB_ERR_WRONG_NO_OF_PARAMS                         -- ^ SqlState = @07001@
  | DB_ERR_DATA_TYPE_ATTR_VIOL                        -- ^ SqlState = @07006@
  | DB_ERR_UNABLE_TO_CONNECT                          -- ^ SqlState = @08001@
  | DB_ERR_CONNECTION_IN_USE                          -- ^ SqlState = @08002@
  | DB_ERR_CONNECTION_NOT_OPEN                        -- ^ SqlState = @08003@
  | DB_ERR_REJECTED_CONNECTION                        -- ^ SqlState = @08004@
  | DB_ERR_CONN_FAIL_IN_TRANS                         -- ^ SqlState = @08007@
  | DB_ERR_COMM_LINK_FAILURE                          -- ^ SqlState = @08S01@
  | DB_ERR_INSERT_VALUE_LIST_MISMATCH                 -- ^ SqlState = @21S01@
  | DB_ERR_DERIVED_TABLE_MISMATCH                     -- ^ SqlState = @21S02@
  | DB_ERR_STRING_RIGHT_TRUNC                         -- ^ SqlState = @22001@
  | DB_ERR_NUMERIC_VALUE_OUT_OF_RNG                   -- ^ SqlState = @22003@
  | DB_ERR_ERROR_IN_ASSIGNMENT                        -- ^ SqlState = @22005@
  | DB_ERR_DATETIME_FLD_OVERFLOW                      -- ^ SqlState = @22008@
  | DB_ERR_DIVIDE_BY_ZERO                             -- ^ SqlState = @22012@
  | DB_ERR_STR_DATA_LENGTH_MISMATCH                   -- ^ SqlState = @22026@
  | DB_ERR_INTEGRITY_CONSTRAINT_VIOL                  -- ^ SqlState = @23000@
  | DB_ERR_INVALID_CURSOR_STATE                       -- ^ SqlState = @24000@
  | DB_ERR_INVALID_TRANS_STATE                        -- ^ SqlState = @25000@
  | DB_ERR_INVALID_AUTH_SPEC                          -- ^ SqlState = @28000@
  | DB_ERR_INVALID_CURSOR_NAME                        -- ^ SqlState = @34000@
  | DB_ERR_SYNTAX_ERROR_OR_ACCESS_VIOL                -- ^ SqlState = @37000@
  | DB_ERR_DUPLICATE_CURSOR_NAME                      -- ^ SqlState = @3C000@
  | DB_ERR_SERIALIZATION_FAILURE                      -- ^ SqlState = @40001@
  | DB_ERR_SYNTAX_ERROR_OR_ACCESS_VIOL2               -- ^ SqlState = @42000@
  | DB_ERR_OPERATION_ABORTED                          -- ^ SqlState = @70100@
  | DB_ERR_UNSUPPORTED_FUNCTION                       -- ^ SqlState = @IM001@
  | DB_ERR_NO_DATA_SOURCE                             -- ^ SqlState = @IM002@
  | DB_ERR_DRIVER_LOAD_ERROR                          -- ^ SqlState = @IM003@
  | DB_ERR_SQLALLOCENV_FAILED                         -- ^ SqlState = @IM004@
  | DB_ERR_SQLALLOCCONNECT_FAILED                     -- ^ SqlState = @IM005@
  | DB_ERR_SQLSETCONNECTOPTION_FAILED                 -- ^ SqlState = @IM006@
  | DB_ERR_NO_DATA_SOURCE_DLG_PROHIB                  -- ^ SqlState = @IM007@
  | DB_ERR_DIALOG_FAILED                              -- ^ SqlState = @IM008@
  | DB_ERR_UNABLE_TO_LOAD_TRANSLATION_DLL             -- ^ SqlState = @IM009@
  | DB_ERR_DATA_SOURCE_NAME_TOO_LONG                  -- ^ SqlState = @IM010@
  | DB_ERR_DRIVER_NAME_TOO_LONG                       -- ^ SqlState = @IM011@
  | DB_ERR_DRIVER_KEYWORD_SYNTAX_ERROR                -- ^ SqlState = @IM012@
  | DB_ERR_TRACE_FILE_ERROR                           -- ^ SqlState = @IM013@
  | DB_ERR_TABLE_OR_VIEW_ALREADY_EXISTS               -- ^ SqlState = @S0001@
  | DB_ERR_TABLE_NOT_FOUND                            -- ^ SqlState = @S0002@
  | DB_ERR_INDEX_ALREADY_EXISTS                       -- ^ SqlState = @S0011@
  | DB_ERR_INDEX_NOT_FOUND                            -- ^ SqlState = @S0012@
  | DB_ERR_COLUMN_ALREADY_EXISTS                      -- ^ SqlState = @S0021@
  | DB_ERR_COLUMN_NOT_FOUND                           -- ^ SqlState = @S0022@
  | DB_ERR_NO_DEFAULT_FOR_COLUMN                      -- ^ SqlState = @S0023@
  | DB_ERR_GENERAL_ERROR                              -- ^ SqlState = @S1000@
  | DB_ERR_MEMORY_ALLOCATION_FAILURE                  -- ^ SqlState = @S1001@
  | DB_ERR_INVALID_COLUMN_NUMBER                      -- ^ SqlState = @S1002@
  | DB_ERR_PROGRAM_TYPE_OUT_OF_RANGE                  -- ^ SqlState = @S1003@
  | DB_ERR_SQL_DATA_TYPE_OUT_OF_RANGE                 -- ^ SqlState = @S1004@
  | DB_ERR_OPERATION_CANCELLED                        -- ^ SqlState = @S1008@
  | DB_ERR_INVALID_ARGUMENT_VALUE                     -- ^ SqlState = @S1009@
  | DB_ERR_FUNCTION_SEQUENCE_ERROR                    -- ^ SqlState = @S1010@
  | DB_ERR_OPERATION_INVALID_AT_THIS_TIME             -- ^ SqlState = @S1011@
  | DB_ERR_INVALID_TRANS_OPERATION_CODE               -- ^ SqlState = @S1012@
  | DB_ERR_NO_CURSOR_NAME_AVAIL                       -- ^ SqlState = @S1015@
  | DB_ERR_INVALID_STR_OR_BUF_LEN                     -- ^ SqlState = @S1090@
  | DB_ERR_DESCRIPTOR_TYPE_OUT_OF_RANGE               -- ^ SqlState = @S1091@
  | DB_ERR_OPTION_TYPE_OUT_OF_RANGE                   -- ^ SqlState = @S1092@
  | DB_ERR_INVALID_PARAM_NO                           -- ^ SqlState = @S1093@
  | DB_ERR_INVALID_SCALE_VALUE                        -- ^ SqlState = @S1094@
  | DB_ERR_FUNCTION_TYPE_OUT_OF_RANGE                 -- ^ SqlState = @S1095@
  | DB_ERR_INF_TYPE_OUT_OF_RANGE                      -- ^ SqlState = @S1096@
  | DB_ERR_COLUMN_TYPE_OUT_OF_RANGE                   -- ^ SqlState = @S1097@
  | DB_ERR_SCOPE_TYPE_OUT_OF_RANGE                    -- ^ SqlState = @S1098@
  | DB_ERR_NULLABLE_TYPE_OUT_OF_RANGE                 -- ^ SqlState = @S1099@
  | DB_ERR_UNIQUENESS_OPTION_TYPE_OUT_OF_RANGE        -- ^ SqlState = @S1100@
  | DB_ERR_ACCURACY_OPTION_TYPE_OUT_OF_RANGE          -- ^ SqlState = @S1101@
  | DB_ERR_DIRECTION_OPTION_OUT_OF_RANGE              -- ^ SqlState = @S1103@
  | DB_ERR_INVALID_PRECISION_VALUE                    -- ^ SqlState = @S1104@
  | DB_ERR_INVALID_PARAM_TYPE                         -- ^ SqlState = @S1105@
  | DB_ERR_FETCH_TYPE_OUT_OF_RANGE                    -- ^ SqlState = @S1106@
  | DB_ERR_ROW_VALUE_OUT_OF_RANGE                     -- ^ SqlState = @S1107@
  | DB_ERR_CONCURRENCY_OPTION_OUT_OF_RANGE            -- ^ SqlState = @S1108@
  | DB_ERR_INVALID_CURSOR_POSITION                    -- ^ SqlState = @S1109@
  | DB_ERR_INVALID_DRIVER_COMPLETION                  -- ^ SqlState = @S1110@
  | DB_ERR_INVALID_BOOKMARK_VALUE                     -- ^ SqlState = @S1111@
  | DB_ERR_DRIVER_NOT_CAPABLE                         -- ^ SqlState = @S1C00@
  | DB_ERR_TIMEOUT_EXPIRED                            -- ^ SqlState = @S1T00@
  | DB_ERR_CONNECT
  deriving (Read,Show,Eq,Enum)

-- | Retrieve the status of the database
dbGetDbStatus :: Db a -> IO DbStatus
dbGetDbStatus db
  = do i <- dbGetStatus db
       if (i < 0 || i >= fromEnum DB_ERR_CONNECT)
        then return DB_FAILURE
        else return (toEnum i)

{----------------------------------------------------------
  Db types
----------------------------------------------------------}
-- | Standard logical database types.
data DbType
  = DbUnknown 
  | DbVarChar         -- ^ Strings
  | DbInteger         
  | DbFloat       
  | DbDate
  | DbBlob            -- ^ Binary
  deriving (Show,Eq,Enum)

-- | Standard SQL types. Use 'toSqlType' and 'fromSqlType' to marshal between
-- the system SQL types (like 'wxSQL_C_CHAR' etc.)
data SqlType
  = SqlChar           -- ^ Fixed Strings
  | SqlNumeric
  | SqlDecimal
  | SqlInteger
  | SqlSmallInt
  | SqlFloat
  | SqlReal
  | SqlDouble
  | SqlDate
  | SqlTime
  | SqlTimeStamp
  | SqlVarChar        -- ^ Strings
  | SqlBit
  | SqlBinary
  | SqlVarBinary
  | SqlBigInt
  | SqlTinyInt
  | SqlUnknown Int    -- ^ Unknown SQL type. Argument specifies the system sql type.
  deriving (Show,Eq)

instance Enum SqlType where
  toEnum i
    = case i of
        1  -> SqlChar           
        2  -> SqlNumeric
        3  -> SqlDecimal
        4  -> SqlInteger
        5  -> SqlSmallInt
        6  -> SqlFloat
        7  -> SqlReal
        8  -> SqlDouble
        9  -> SqlDate
        10 -> SqlTime
        11 -> SqlTimeStamp
        12 -> SqlVarChar
        13 -> SqlBit
        14 -> SqlBinary
        15 -> SqlVarBinary
        16 -> SqlBigInt
        17 -> SqlTinyInt
        _  -> SqlUnknown i

  fromEnum tp
    = case tp of
        SqlChar       -> 1
        SqlNumeric    -> 2
        SqlDecimal    -> 3
        SqlInteger    -> 4
        SqlSmallInt   -> 5
        SqlFloat      -> 6
        SqlReal       -> 7
        SqlDouble     -> 8
        SqlDate       -> 9
        SqlTime       -> 10 
        SqlTimeStamp  -> 11
        SqlVarChar    -> 12
        SqlBit        -> 13
        SqlBinary     -> 14
        SqlVarBinary  -> 15
        SqlBigInt     -> 16
        SqlTinyInt    -> 17 
        SqlUnknown i  -> i

-- | Convert a system SQL type (like 'wxSQL_C_CHAR') to a standard 'SqlType'.
toSqlType :: Int -> SqlType
toSqlType i
  = unsafePerformIO $
    do tp <- dbSqlTypeToStandardSqlType i
       return (toEnum tp)

-- | Convert to a system SQL type (like 'wxSQL_C_INTEGER') from a standard 'SqlType'.
fromSqlType :: SqlType -> Int
fromSqlType tp
  = unsafePerformIO (dbStandardSqlTypeToSqlType (fromEnum tp))
