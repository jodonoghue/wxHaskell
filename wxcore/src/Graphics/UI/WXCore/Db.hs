--------------------------------------------------------------------------------
{-| Module      :  Controls
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

> printAuthorNames
>   = do names <- dbWithConnection "pubs" "" "" (\db ->
>                  dbQuery db "SELECT au_fname, au_lname FROM authors" 
>                    (\row -> do fname <- dbRowGetStringValue row "au_fname"
>                                lname <- dbRowGetStringValue row "au_lname"
>                                return (fname ++ " " ++ lname)
>                    ))
>        putStrLn (unlines names)

-}
--------------------------------------------------------------------------------
module Graphics.UI.WXCore.Db
   ( 
   -- * Connection
      dbWithConnection, dbConnect, dbDisconnect
   
   -- * Queries
   , dbQuery, dbQuery_, dbExecute

   -- * Result sets
   , DbRow, DbValue( dbValueRead )
   , dbRowGetValue, dbRowGetNullValue
   , dbRowGetStringValue, dbRowGetStringNullValue
   , dbRowGetColumnInfo, dbRowGetColumnInfos
     
   -- * Meta information
   -- ** Data sources
   , DatasourceName, dbGetDataSources, dbGetDatasourceInfo
   
   -- ** Dbms
   , Dbms(..), dbGetDbms
   
   -- ** Tables and columns
   , TableName, ColumnName, ColumnIndex
   , DbInfo(..), TableInfo(..), ColumnInfo(..)
   , dbGetInfo, dbGetTableColumnInfos, dbGetColumnInfos

   -- * Exceptions
   , DbStatus(..), dbGetDbStatus
   , DbError(..)
   , catchDbError, raiseDbError
   , dbHandleExn, dbCheckExn, dbRaiseExn
   , dbGetErrorMessages
   
   -- * Sql types
   , DbType(..), SqlType(..), toSqlType, fromSqlType
   ) where


import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types

import IO( catch, ioError, isUserError, ioeGetErrorString)
import List( isPrefixOf )
import Char( isDigit )
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array



{----------------------------------------------------------
  Query
----------------------------------------------------------}
dbQuery  :: Db a -> String -> (DbRow a -> IO b) -> IO [b]
dbQuery db select action
  = do dbExecute db select
       infos <- dbGetColumnInfos db
       walkRows (DbRow db infos) [] 
  where
    walkRows row acc
      = do ok <- dbGetNext db
           if (not ok)
            then return (reverse acc)
            else do x <- action row
                    walkRows row (x:acc)


dbQuery_ :: Db a -> String -> (DbRow a -> IO b) -> IO ()
dbQuery_ db select action
  = do dbHandleExn db $ dbExecSql db select
       infos <- dbGetColumnInfos db
       walkRows (DbRow db infos) 
  where
    walkRows row 
      = do ok <- dbGetNext db
           if (not ok)
            then return ()
            else do action row
                    walkRows row 

-- | Execute a SQL statement against the database. Raises a 'DbError'
-- on failure.
dbExecute :: Db a -> String -> IO ()
dbExecute db sql
  = dbHandleExn db $ dbExecSql db sql


{----------------------------------------------------------
  Result rows of a query
----------------------------------------------------------}
data DbRow a = DbRow (Db a) [ColumnInfo]

dbRowGetColumnInfos :: DbRow a -> [ColumnInfo]
dbRowGetColumnInfos (DbRow db columnInfos)
  = columnInfos

dbRowGetNullValue   :: DbValue b => DbRow a -> ColumnName -> IO (Maybe b)
dbRowGetNullValue row@(DbRow db columnInfos) name
  = do info <- dbRowGetColumnInfo row name
       dbValueRead db info 

dbRowGetColumnInfo :: DbRow a -> ColumnName -> IO ColumnInfo
dbRowGetColumnInfo (DbRow db columnInfos) name
  = case lookup name (zip (map columnName columnInfos) columnInfos) of
      Just info -> return info
      Nothing   -> raiseDbInvalidColumnName db name

dbRowGetValue :: DbValue b => DbRow a -> ColumnName -> IO b
dbRowGetValue row@(DbRow db columnInfos) columnName
  = do mbValue <- dbRowGetNullValue row columnName
       case mbValue of
         Just x  -> return x
         Nothing -> raiseDbFetchNull db

class DbValue a where
  dbValueRead :: Db b -> ColumnInfo -> IO (Maybe a)

instance DbValue Bool where
  dbValueRead db columnInfo 
    = alloca $ \pint ->
      do isNull <- dbGetDataNull db $ dbGetDataInt db (columnIndex columnInfo) pint
         if isNull 
          then return Nothing
          else do i <- peek pint
                  return (Just (i/=0))


instance DbValue Int where
  dbValueRead db columnInfo 
    = alloca $ \pint ->
      do isNull <- dbGetDataNull db $ dbGetDataInt db (columnIndex columnInfo) pint
         if isNull 
          then return Nothing
          else do i <- peek pint
                  return (Just (fromCInt i))

instance DbValue Double where
  dbValueRead db columnInfo 
    = alloca $ \pdouble ->
      do isNull <- dbGetDataNull db $ dbGetDataDouble db (columnIndex columnInfo) pdouble
         if isNull 
          then return Nothing
          else do d <- peek pdouble
                  return (Just d)


instance DbValue Integer where
  dbValueRead db columnInfo 
    = do mbS <- dbStringRead db columnInfo 
         case mbS of
           Nothing -> return Nothing
           Just s  -> case parse s of
                        Just i  -> return (Just i)
                        Nothing -> raiseDbTypeMismatch db 
    where
      parse s
        = let (val,xs) = span isDigit s
          in case xs of
               ('.':frac) | all isDigit frac 
                          -> Just (read (val ++ adjust (columnDecimalDigits columnInfo) frac))
               other      -> Nothing


dbRowGetStringValue :: DbRow a -> ColumnName -> IO String
dbRowGetStringValue row name
  = do mbStr <- dbRowGetStringNullValue row name
       return (maybe "" id mbStr)

dbRowGetStringNullValue :: DbRow a -> ColumnName -> IO (Maybe String)
dbRowGetStringNullValue row@(DbRow db columnInfos) name
  = do info <- dbRowGetColumnInfo row name
       dbStringRead db info 


dbStringRead :: Db a -> ColumnInfo -> IO (Maybe String)
dbStringRead db info 
  = allocaArray maxLen $ \cstr ->
    do isNull <- dbGetDataNull db $ dbGetDataString db (columnIndex info) cstr maxLen 
       if isNull
        then return Nothing
        else do s <- peekCString cstr 
                return (Just s)
  where
    maxLen  | columnSize info > 0   = columnSize info + 1
            | otherwise             = 4096  -- just something ?

-- | Takes a @dbGetData...@ method and supplies the @Ptr CInt@ argument.
-- It raises and exception on error. Otherwise, it returns 'True' when a
-- @NULL@ value is read.
dbGetDataNull :: Db a -> (Ptr CInt -> IO Bool) -> IO Bool
dbGetDataNull db getData
  = alloca $ \pused ->
    do dbHandleExn db $ getData pused
       used <- peek pused
       return (fromCInt used == wxSQL_NULL_DATA)
    
{----------------------------------------------------------
  Open connection
----------------------------------------------------------}
{- | Open a connection and automatically close it after the computation
  returns. Takes the name of the data source, a user name, and password as
   arguments. Raises a database exception ('DbError') when the connection
   fails.
-}
dbWithConnection :: DatasourceName -> String -> String -> (Db () -> IO b) -> IO b
dbWithConnection name userid password f
  = bracket (dbConnect name userid password)
            (dbDisconnect)
            (f)

-- | (@dbConnect name userId password@) creates a connection to a 
-- data source @name@. Raises a database exception ('DbError') when the connection fails.
-- Use 'dbDisconnect' to close the connection.
dbConnect :: DatasourceName -> String -> String -> IO (Db ())
dbConnect name userId password
  = bracket (dbConnectInfCreate nullHENV name userId password "" "" "" )
            (dbConnectInfDelete)
            (\connectInf -> 
              do db <- dbGetConnection connectInf True
                 if objectIsNull db 
                  then raiseDbConnect name
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
type DatasourceName = String
type TableName      = String
type ColumnName     = String
type ColumnIndex    = Int

-- | Database information.
data DbInfo
  = DbInfo   { dbCatalog  :: String       -- ^ System name of the database
             , dbSchema   :: String       -- ^ Schema name
             , dbTables   :: [TableInfo]  -- ^ The tables of the database
             }

-- | Database table information.
data TableInfo
  = TableInfo{ tableName    :: TableName     -- ^ Name of the table.
             , tableType    :: String        -- ^ Type of the table (ie. @SYSTEM TABLE@, @TABLE@, etc)
             , tableRemarks :: String        -- ^ Comments
             , tableColumns :: [ColumnInfo]  -- ^ The columns of the table.
             }

-- | Database column information.
data ColumnInfo
  = ColumnInfo{ columnName     :: ColumnName  -- ^ Column name.
              , columnIndex    :: ColumnIndex -- ^ 1-based column index.
              , columnSize     :: Int     -- ^ Length of the column.
              , columnNullable :: Bool    -- ^ Are NULL values allowed?  
              , columnType     :: DbType  -- ^ Logical type
              , columnSqlType  :: SqlType -- ^ SQL type
              , columnTypeName :: String  -- ^ SQL type name (ie. @VARCHAR@, @INTEGER@ etc.)
              , columnRemarks  :: String  -- ^ Comments

              , columnDecimalDigits    :: Int     -- ^ Number of decimal digits
              , columnNumPrecRadix     :: Int     -- ^ Radix precision
              , columnIsForeignKey     :: Bool    -- ^ Is this a foreign key column?
              , columnIsPrimaryKey     :: Bool    -- ^ Is this a primary key column?
              , columnForeignTableName :: String  -- ^ Tables that use this foreign key.
              , columnPrimaryTableName :: String  -- ^ Tables that use this primary key.
              }


-- | Get the complete meta information of a data source. Takes the
-- data source name, a user id, and password as arguments.
--
-- > do info <- dbGetDatasourceInfo "pubs" "" ""
-- >    print info
-- 
dbGetDatasourceInfo :: DatasourceName -> String -> String -> IO DbInfo
dbGetDatasourceInfo dataSource userId password
  = bracket (dbConnectInfCreate nullHENV dataSource userId password "" "" "") 
            (dbConnectInfDelete)
            (\connectInf ->
             do henv <- dbConnectInfGetHenv connectInf
                db   <- dbCreate henv True
                if (objectIsNull db)
                 then raiseDbConnect dataSource
                 else do dbOpen db dataSource userId password
                         opened <- dbIsOpen db
                         if (not opened)
                          then do dbDelete db
                                  dbRaiseExn db
                          else do info <- dbGetInfo db
                                  dbClose db
                                  dbDelete db
                                  return info)

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
       columns   <- dbGetTableColumnInfos db tableName  
       return (TableInfo tableName tableType remarks columns)

-- | Return the column information of the current query.
dbGetColumnInfos :: Db a -> IO [ColumnInfo]
dbGetColumnInfos db
  = alloca $ \pcnumCols ->
    bracket (dbGetResultColumns db pcnumCols)
            (dbColInfArrayDelete)
            (\colInfs  -> do cnumCols <- peek pcnumCols
                             let numCols = fromCInt cnumCols
                             mapM (\idx -> do colInf <- dbColInfArrayGetColInf colInfs (idx-1)
                                              dbColInfGetInfo colInf idx) 
                                  [1..numCols])
    

-- | Return the column information of a certain table. Use an empty
-- table name to get the column information of the current query
-- ('dbGetColumnInfos').
dbGetTableColumnInfos :: Db a -> TableName -> IO [ColumnInfo]
dbGetTableColumnInfos db tableName
  | null tableName = dbGetColumnInfos db
  | otherwise =
    alloca $ \pcnumCols ->
    bracket (dbGetColumns db tableName pcnumCols "")
            (dbColInfArrayDelete)
            (\colInfs  -> do cnumCols <- peek pcnumCols
                             let numCols = fromCInt cnumCols
                             mapM (\idx -> do colInf <- dbColInfArrayGetColInf colInfs (idx-1)
                                              dbColInfGetInfo colInf idx) 
                                  [1..numCols])
        
             
dbColInfGetInfo :: DbColInf a -> ColumnIndex -> IO ColumnInfo
dbColInfGetInfo info idx
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
       return (ColumnInfo columnName idx columnSize nullable (toEnum tp) (toSqlType sqltp) tpname remarks
                          decdigits numprecrad (fk/=0) (pk/=0) fkname pkname )


{----------------------------------------------------------
  Data sources
----------------------------------------------------------}
-- | Returns the name and description of the data sources on the system.
dbGetDataSources :: IO [(DatasourceName,String)]
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
  = DbmsORACLE 
  | DbmsSYBASE_ASA         -- ^ Adaptive Server Anywhere
  | DbmsSYBASE_ASE         -- ^ Adaptive Server Enterprise
  | DbmsMS_SQL_SERVER 
  | DbmsMY_SQL 
  | DbmsPOSTGRES 
  | DbmsACCESS 
  | DbmsDBASE 
  | DbmsINFORMIX 
  | DbmsVIRTUOSO 
  | DbmsDB2 
  | DbmsINTERBASE 
  | DbmsPERVASIVE_SQL 
  | DbmsXBASE_SEQUITER 
  | DbmsUNIDENTIFIED 
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
-- | Database error type.
data DbError
  = DbError   { dbErrorMsg   :: String
              , dbDataSource :: DatasourceName
              , dbErrorCode  :: DbStatus 
              , dbNativeCode :: Int
              , dbSqlState   :: String  
              }    -- ^ General error.
  deriving (Read,Show)  


-- | Automatically raise a database exception when 'False' is returned.
-- You can use this method around basic database methods to conveniently
-- throw Haskell exceptions.
--
-- >  dbHandleExn db $ dbExecSql db "SELECT au_fname FROM authors"           
--
dbHandleExn :: Db a -> IO Bool -> IO ()
dbHandleExn db io
  = do ok <- io
       if ok
        then return ()
        else dbRaiseExn db

-- | Raise a database exception based on the current error status of
-- the database. Does nothing when no error is set.
dbCheckExn :: Db a -> IO ()
dbCheckExn db
  = do status <- dbGetDbStatus db
       if (status == DB_SUCCESS)
        then return ()
        else dbRaiseExn db

-- | Raise a database exception based on the current error status of
-- the database.
dbRaiseExn :: Db a -> IO b
dbRaiseExn db
  = do errorMsg  <- dbGetErrorMessage db 0
       errorCode <- dbGetDbStatus db
       nativeCode<- dbGetNativeError db
       dataSource<- dbGetDatasourceName db
       raiseDbError (DbError (extractMessage errorMsg) dataSource errorCode nativeCode (extractSqlState errorMsg))
  where
    extractSqlState msg
      | isPrefixOf sqlStatePrefix msg = takeWhile (/='\n') (drop (length sqlStatePrefix) msg)
      | otherwise                     = ""
      where
        sqlStatePrefix  = "SQL State = "

    extractMessage msg
      = dropTillPrefix "Error Message = " msg

    dropTillPrefix prefix msg
      = walk msg
      where
        walk s  | null s                = msg
                | isPrefixOf prefix s   = drop (length prefix) s
                | otherwise             = walk (tail s)

-- | Get the raw error message history. More recent error messages
-- come first.
dbGetErrorMessages :: Db a -> IO [String]
dbGetErrorMessages db
  = do n <- dbGetNumErrorMessages db
       mapM (\idx -> dbGetErrorMessage db (idx-1)) [1..n]

-- | Raise a type mismatch error
raiseDbTypeMismatch :: Db a -> IO b
raiseDbTypeMismatch db
  = do dataSource <- dbGetDatasourceName db
       raiseDbError (DbError "Type mismatch" dataSource DB_ERR_TYPE_MISMATCH 0 "" )

-- | Raise a fetch null error
raiseDbFetchNull :: Db a -> IO b
raiseDbFetchNull db
  = do dataSource <- dbGetDatasourceName db
       raiseDbError (DbError "Unexpected NULL value" dataSource DB_ERR_FETCH_NULL 0 "")

-- | Raise an invalid column name error
raiseDbInvalidColumnName :: Db a -> ColumnName -> IO b
raiseDbInvalidColumnName db name
  = do dataSource <- dbGetDatasourceName db
       raiseDbError (DbError ("Invalid column name (" ++ name ++ ")") dataSource DB_ERR_INVALID_COLUMN_NAME 0 "")

-- | Raise a connection error
raiseDbConnect :: DatasourceName -> IO a
raiseDbConnect name
  = raiseDbError (DbError ("Unable to establish a connection to the '" ++ name ++ "' database") 
                 name DB_ERR_CONNECT 0 "")

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
  | DB_ERR_FETCH_NULL          -- ^ Unexpected NULL value
  | DB_ERR_INVALID_COLUMN_NAME -- ^ Invalid (or unknown) column name
  | DB_ERR_TYPE_MISMATCH       -- ^ Trying to convert a SQL value of the wrong type
  | DB_ERR_CONNECT             -- ^ Unable to establish a connection
  deriving (Read,Show,Eq,Enum)

-- | Retrieve the current status of the database
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

-- | Standard SQL types. 
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


{----------------------------------------------------------
   Print meta information about  a particular data source
----------------------------------------------------------}
instance Show DbInfo where
  show info   = unlines (showDbInfo info)

showDbInfo :: DbInfo -> [String]
showDbInfo info
  = ["catalog: " ++ dbCatalog info
    ,"schema : " ++ dbSchema info
    ,"tables : "
    ] ++ 
    numbered (map showTableInfo (dbTables info))


instance Show TableInfo where
  show info   = unlines (showTableInfo info)

showTableInfo :: TableInfo -> [String]
showTableInfo info
  = ["name   : " ++ tableName info
    ,"type   : " ++ tableType info
    ,"remarks: " ++ tableRemarks info
    ,"columns: "
    ] ++ showColumnInfos (tableColumns info)
    

instance Show ColumnInfo where
  show info       = unlines (showColumnInfo info)
  showList infos  = showString (unlines (showColumnInfos infos))

showColumnInfos infos
  = numbered (map showColumnInfo infos)

showColumnInfo info
  = ["name   : " ++ columnName info
    ,"index  : " ++ show (columnIndex info)
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
