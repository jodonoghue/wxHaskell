--------------------------------------------------------------------------------
{-| Module      :  Db
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable
  
This module provides convenient access to the database classes
('Db') of wxWindows. These classes have been donated to the wxWindows
library by Remstar International. (Note: these classes are not supported
on MacOS X at the time of writing (november 2003)). 
These database objects support ODBC connections and have been tested 
with wxWindows on the following databases:

Oracle (v7, v8, v8i), 
Sybase (ASA and ASE),
MS SQL Server (v7 - minimal testing), MS Access (97 and 2000),
MySQL, DBase (IV, V) (using ODBC emulation), PostgreSQL, INFORMIX, VIRTUOSO, DB2, 
Interbase, Pervasive SQL .

The database functions also work with console applications and do /not/
need to initialize the WXCore libraries.

The examples in this document are all based on the @pubs@ database
that is available in MS Access 97 and \'comma separated text\' format
from <http://wxhaskell.sourceforge.net/download/pubs.zip>. We assume
that your system is configured in such a way that @pubs@ is the
datasource name of this database. (On Windows XP for example, this is 
done using the /start - settings - control panel - administrative tools
- data sources (ODBC)/ menu.)

The available data sources on your system can be retrieved using
'dbGetDataSources'. Here is an example from my system:

> *Main> dbGetDataSources >>= print
> [("pubs","Microsoft Access Driver (*.mdb)")]

Connections are established with the 'dbWithConnection' call.
It takes a datasource name, a user name, a password, and a function
that is applied to the resulting database connection:

> dbWithConnection "pubs" "" "" (\db -> ...)

(Note that most database operations automatically raise a database exception ('DbError')
on failure. These exceptions can be caught using 'catchDbError'.)

The resulting database ('Db') can be queried using 'dbQuery'.
The 'dbQuery' call applies a function to each row ('DbRow') in the result
set. Using calls like 'dbRowGetValue' and 'dbRowGetString', you can 
retrieve the values from the result rows.

> printAuthorNames
>   = do names <- dbWithConnection "pubs" "" "" (\db ->
>                  dbQuery db "SELECT au_fname, au_lname FROM authors" 
>                    (\row -> do fname <- dbRowGetString row "au_fname"
>                                lname <- dbRowGetString row "au_lname"
>                                return (fname ++ " " ++ lname)
>                    ))
>        putStrLn (unlines names)

The overloaded function 'dbRowGetValue' can retrieve any kind of 
database value ('DbValue') (except for strings since standard Haskell98 
does not support overlapping instances). For most datatypes, there is
also a non-overloaded version, like 'dbRowGetInteger' and 'dbRowGetString'.
The @dbRowGet...@ functions are also available as @dbRowGet...Mb@, which
returns 'Nothing' when a @NULL@ value is encountered (instead of raising
an exception), for example, 'dbRowGetIntegerMb' and 'dbRowGetStringMb'.

If necessary, more data types can be supported by defining your own
'DbValue' instances and using 'dbRowGetValue' to retrieve those values.

You can use 'dbRowGetColumnInfo' to retrieve column information ('ColumnInfo')
about a particular column, for example, to retieve the number of decimal
digits in a currency value.

Complete meta information about a particular data source can be retrieved 
using 'dbGetDataSourceInfo', that takes a data source name, user name,
and password as arguments, and returns a 'DbInfo' structure:

> *Main> dbGetDataSourceInfo "pubs" "" "" >>= print
> catalog: C:\daan\temp\db\pubs2
> schema :
> tables :
>  ...
>  8: name   : authors
>     type   : TABLE
>     remarks:
>     columns:
>      1: name   : au_id
>         index  : 1
>         type   : VARCHAR
>         size   : 12
>         sqltp  : SqlVarChar
>         type id: DbVarChar
>         digits : 0
>         prec   : 0
>         remarks: Author Key
>         pkey   : 0
>         ptable :
>         fkey   : 0
>         ftable :
>      2: name   : au_fname
>         index  : 2
>         type   : VARCHAR
>  ...

-}
--------------------------------------------------------------------------------
module Graphics.UI.WXCore.Db
   ( 
   -- * Connection
      dbWithConnection, dbConnect, dbDisconnect
   , dbWithDirectConnection, dbConnectDirect
   -- * Queries
   , dbQuery, dbQuery_, dbExecute

   -- * Rows
   , DbRow(..)

   -- ** Standard values
   , dbRowGetString, dbRowGetStringMb
   , dbRowGetBool, dbRowGetBoolMb
   , dbRowGetInt, dbRowGetIntMb
   , dbRowGetDouble, dbRowGetDoubleMb
   , dbRowGetInteger, dbRowGetIntegerMb
   , dbRowGetClockTime, dbRowGetClockTimeMb

   -- ** Generic values
   , DbValue( dbValueRead, toSqlValue )
   , dbRowGetValue, dbRowGetValueMb
   
   -- ** Column information
   , dbRowGetColumnInfo, dbRowGetColumnInfos
     
   -- * Meta information
   -- ** Data sources
   , DataSourceName, dbGetDataSources
   , dbGetDataSourceInfo, dbGetDataSourceTableInfo
   
   -- ** Tables and columns
   , TableName, ColumnName, ColumnIndex
   , DbInfo(..), TableInfo(..), ColumnInfo(..)
   , dbGetInfo, dbGetTableInfo, dbGetTableColumnInfos, dbGetColumnInfos

   -- ** Dbms
   , Dbms(..), dbGetDbms   

   -- * Exceptions
   , DbError(..)
   , catchDbError, raiseDbError
   , dbHandleExn, dbCheckExn, dbRaiseExn
   , dbGetErrorMessages
   , dbGetDbStatus, DbStatus(..)
   
   -- * Sql types
   , DbType(..), SqlType(..)
   , toSqlTableName, toSqlColumnName
   , toSqlString, toSqlTime, toSqlDate, toSqlTimeStamp

   -- * Internal
   , dbStringRead, dbGetDataNull, toSqlType, fromSqlType
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
import System.Time


{----------------------------------------------------------
  Query
----------------------------------------------------------}
-- | Execute a SQL query against a database. Takes a function
-- as argument that is applied to every database row ('DbRow').
-- The results of these applications are returned as a list.
-- Raises a 'DbError' on failure.
--
-- >  do names <- dbQuery db "SELECT au_fname FROM authors" 
-- >                (\row -> dbRowGetString row "au_fname")
-- >     putStr (unlines names)
--
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

-- | Execute a SQL query against a database. Takes a function
-- as argument that is applied to every row in the database.
-- Raises a 'DbError' on failure.
--
-- >  dbQuery_ db "SELECT au_fname FROM authors" 
-- >    (\row -> do fname <- dbRowGetString row "au_fname"
-- >                putStrLn fname)
--
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
-- | An abstract database row.
data DbRow a = DbRow (Db a) [ColumnInfo]

-- | Get the column information of a row.
dbRowGetColumnInfos :: DbRow a -> [ColumnInfo]
dbRowGetColumnInfos (DbRow db columnInfos)
  = columnInfos

-- | The column information of a particular column. 
-- Raises a 'DbError' on failure.
dbRowGetColumnInfo :: DbRow a -> ColumnName -> IO ColumnInfo
dbRowGetColumnInfo (DbRow db columnInfos) name
  = case lookup name (zip (map columnName columnInfos) columnInfos) of
      Just info -> return info
      Nothing   -> if (all isDigit name)
                    then case lookup (read name) (zip (map columnIndex columnInfos) columnInfos) of
                           Just info -> return info
                           Nothing   -> err
                    else err
  where
    err = raiseDbInvalidColumnName db (name ++ " in " ++ (show (map columnName columnInfos)))

-- | Get a database value ('DbValue') from a row.
-- Returns 'Nothing' when a @NULL@ value is encountered.
-- Raises a 'DbError' on failure.
dbRowGetValueMb   :: DbValue b => DbRow a -> ColumnName -> IO (Maybe b)
dbRowGetValueMb row@(DbRow db columnInfos) name
  = do info <- dbRowGetColumnInfo row name
       dbValueRead db info 

-- | Get a database value ('DbValue') from a row. 
-- Raises a 'DbError' on failure or when a @NULL@ value is encountered.
dbRowGetValue :: DbValue b => DbRow a -> ColumnName -> IO b
dbRowGetValue row@(DbRow db columnInfos) columnName
  = do mbValue <- dbRowGetValueMb row columnName
       case mbValue of
         Just x  -> return x
         Nothing -> raiseDbFetchNull db

-- | Class of values that are supported by the database.
class DbValue a where
  -- | Read a value at a specified column from the database. 
  -- Return 'Nothing' when a @NULL@ value is encountered
  -- Raises a 'DbError' on failure. ('dbGetDataNull' can be
  -- used when implementing this behaviour).
  dbValueRead :: Db b -> ColumnInfo -> IO (Maybe a)

  -- | Convert a value to a string representation that can be
  -- used directly in a SQL statement.
  toSqlValue  :: a -> String

instance DbValue Bool where
  dbValueRead db columnInfo 
    = alloca $ \pint ->
      do isNull <- dbGetDataNull db $ dbGetDataInt db (columnIndex columnInfo) pint
         if isNull 
          then return Nothing
          else do i <- peek pint
                  return (Just (i/=0))
  toSqlValue b
    = if b then "TRUE" else "FALSE"

instance DbValue Int where
  dbValueRead db columnInfo 
    = alloca $ \pint ->
      do isNull <- dbGetDataNull db $ dbGetDataInt db (columnIndex columnInfo) pint
         if isNull 
          then return Nothing
          else do i <- peek pint
                  return (Just (fromCInt i))
  toSqlValue i
    = show i

instance DbValue Double where
  dbValueRead db columnInfo 
    = alloca $ \pdouble ->
      do isNull <- dbGetDataNull db $ dbGetDataDouble db (columnIndex columnInfo) pdouble
         if isNull 
          then return Nothing
          else do d <- peek pdouble
                  return (Just d)

  toSqlValue d
    = show d

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

  toSqlValue i
    = show i


instance DbValue ClockTime where
  dbValueRead db columnInfo
    = alloca $ \pfraction ->
      alloca $ \psecs ->
      do poke pfraction (toCInt 0)
         isNull <- dbGetDataNull db $
                   case columnSqlType columnInfo of
                     SqlDate -> dbGetDataDate db (columnIndex columnInfo) psecs
                     SqlTime -> dbGetDataTime db (columnIndex columnInfo) psecs
                     other   -> dbGetDataTimeStamp db (columnIndex columnInfo) psecs pfraction
         if (isNull)
          then return Nothing
          else do secs     <- peek psecs
                  fraction <- peek pfraction
                  return (Just (TOD (fromIntegral secs) (fromIntegral fraction * 1000)))

  toSqlValue ctime
    = toSqlTimeStamp ctime


-- | Read an 'Bool' from the database. 
-- Raises a 'DbError' on failure or when a @NULL@ value is encountered.
dbRowGetBool :: DbRow a -> ColumnName -> IO Bool
dbRowGetBool = dbRowGetValue

-- | Read an 'Bool' from the database. 
-- Returns 'Nothing' when a @NULL@ value is encountered.
-- Raises a 'DbError' on failure.
dbRowGetBoolMb :: DbRow a -> ColumnName -> IO (Maybe Bool)
dbRowGetBoolMb = dbRowGetValueMb


-- | Read an 'Int' from the database. 
-- Raises a 'DbError' on failure or when a @NULL@ value is encountered.
dbRowGetInt :: DbRow a -> ColumnName -> IO Int
dbRowGetInt = dbRowGetValue

-- | Read an 'Int' from the database. 
-- Returns 'Nothing' when a @NULL@ value is encountered.
-- Raises a 'DbError' on failure.
dbRowGetIntMb :: DbRow a -> ColumnName -> IO (Maybe Int)
dbRowGetIntMb = dbRowGetValueMb


-- | Read an 'Double' from the database. 
-- Raises a 'DbError' on failure or when a @NULL@ value is encountered.
dbRowGetDouble :: DbRow a -> ColumnName -> IO Double
dbRowGetDouble = dbRowGetValue

-- | Read an 'Double' from the database. 
-- Returns 'Nothing' when a @NULL@ value is encountered.
-- Raises a 'DbError' on failure.
dbRowGetDoubleMb :: DbRow a -> ColumnName -> IO (Maybe Double)
dbRowGetDoubleMb = dbRowGetValueMb


-- | Read an 'Integer' from the database. 
-- Raises a 'DbError' on failure or when a @NULL@ value is encountered.
dbRowGetInteger :: DbRow a -> ColumnName -> IO Integer
dbRowGetInteger = dbRowGetValue

-- | Read an 'Integer' from the database. 
-- Returns 'Nothing' when a @NULL@ value is encountered.
-- Raises a 'DbError' on failure.
dbRowGetIntegerMb :: DbRow a -> ColumnName -> IO (Maybe Integer)
dbRowGetIntegerMb = dbRowGetValueMb

-- | Read an 'ClockTime' from the database (from a SQL Time, TimeStamp, or Date field). 
-- Raises a 'DbError' on failure or when a @NULL@ value is encountered.
dbRowGetClockTime :: DbRow a -> ColumnName -> IO ClockTime
dbRowGetClockTime = dbRowGetValue

-- | Read an 'ClockTime' from the database (from a SQL Time, TimeStamp, or Date field). 
-- Returns 'Nothing' when a @NULL@ value is encountered.
-- Raises a 'DbError' on failure.
dbRowGetClockTimeMb :: DbRow a -> ColumnName -> IO (Maybe ClockTime)
dbRowGetClockTimeMb = dbRowGetValueMb


-- | Read a string value from the database. Returns the empty
-- string when a @NULL@ value is encountered.
-- Raises a 'DbError' on failure.
dbRowGetString :: DbRow a -> ColumnName -> IO String
dbRowGetString row name
  = do mbStr <- dbRowGetStringMb row name
       return (maybe "" id mbStr)

-- | Read a string from the database. Returns 'Nothing' when a
-- @NULL@ value is encountered.
-- Raises a 'DbError' on failure.
dbRowGetStringMb :: DbRow a -> ColumnName -> IO (Maybe String)
dbRowGetStringMb row@(DbRow db columnInfos) name
  = do info <- dbRowGetColumnInfo row name
       dbStringRead db info 

-- | Low level string reading.
dbStringRead :: Db a -> ColumnInfo -> IO (Maybe String)
dbStringRead db info 
  = alloca $ \pbuf ->
    alloca $ \plen ->
    do dbHandleExn db $ dbGetDataBinary db (columnIndex info) True pbuf plen
       len <- peek plen
       if (fromCInt len == wxSQL_NULL_DATA)
        then do buf <- peek pbuf
                wxcFree buf
                return Nothing
        else do buf <- peek pbuf
                s   <- peekCStringLen (buf,fromCInt len)
                wxcFree buf
                return (Just s)

-- | Convert a string to SQL string
toSqlString :: String -> String
toSqlString s
  = "'" ++ concatMap quote s ++ "'"
  where
    quote '\''  = "''"
    quote c     = [c]

-- | Convert a 'ClockTime' to a SQL date string (without hours\/minutes\/seconds).
toSqlDate :: ClockTime -> String    
toSqlDate ctime
    = "'" ++ show (ctYear t) ++ "-" ++ show (ctMonth t) ++ "-" ++ show (ctDay t) ++ "'"
    where
      t = toUTCTime ctime

-- | Convert a 'ClockTime' to a SQL full date (timestamp) string.
toSqlTimeStamp :: ClockTime -> String    
toSqlTimeStamp ctime
    = "'" ++ show (ctYear t) ++ "-" ++ show (ctMonth t) ++ "-" ++ show (ctDay t)
      ++ " " ++ show (ctHour t) ++ ":" ++ show (ctMin t) ++ ":" ++ show (ctSec t) ++ "'"
    where
      t = toUTCTime ctime

-- | Convert a 'ClockTime' to a SQL time string (without year\/month\/day).
toSqlTime :: ClockTime -> String    
toSqlTime ctime
    = "'" ++ show (ctHour t) ++ ":" ++ show (ctMin t) ++ ":" ++ show (ctSec t) ++ "'"
    where
      t = toUTCTime ctime


-- | Internal: used to implement 'dbReadValue' methods.
-- Takes a @dbGetData...@ method and supplies the @Ptr CInt@ argument.
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
{- | Open a (cached) connection and automatically close it after the computation
  returns. Takes the name of the data source, a user name, and password as
   arguments. Raises a database exception ('DbError') when the connection
   fails.
-}
dbWithConnection :: DataSourceName -> String -> String -> (Db () -> IO b) -> IO b
dbWithConnection name userid password f
  = bracket (dbConnect name userid password)
            (dbDisconnect)
            (f)


{- | Open a direct database connection and automatically close it after the computation
  returns. This method is not recommended in general as
-- the 'dbWithConnection' function is potentially much more efficient since it
-- caches database connections and meta information, greatly reducing network traffic.
-}
dbWithDirectConnection :: DataSourceName -> String -> String -> (Db () -> IO b) -> IO b
dbWithDirectConnection name userid password f
  = bracket (dbConnectDirect name userid password)
            (\db -> do{ dbClose db; dbDelete db } )
            (f)


-- | (@dbConnect name userId password@) creates a (cached) connection to a 
-- data source @name@. Raises a database exception ('DbError') when the connection fails.
-- Use 'dbDisconnect' to close the connection.
dbConnect :: DataSourceName -> String -> String -> IO (Db ())
dbConnect name userId password
  = bracket (dbConnectInfCreate nullHENV name userId password "" "" "" )
            (dbConnectInfDelete)
            (\connectInf -> 
              do db <- dbGetConnection connectInf True
                 if objectIsNull db 
                  then dbConnectDirect name userId password
                  else do opened <- dbIsOpen db
                          if (not opened)
                           then do dbFreeConnection db
                                   dbConnectDirect name userId password
                           else return db)

-- | Open a direct database connection. This method is in general not recommended as
-- the 'dbConnect' function is potentially much more efficient since it
-- caches database connections and meta information, greatly reducing network traffic.
dbConnectDirect :: DataSourceName -> String -> String -> IO (Db ())
dbConnectDirect dataSource userId password 
  = bracket (dbConnectInfCreate nullHENV dataSource userId password "" "" "") 
            (dbConnectInfDelete)
            (\connectInf ->
             do henv <- dbConnectInfGetHenv connectInf
                db   <- dbCreate henv True
                if (objectIsNull db)
                 then raiseDbConnect dataSource
                 else do opened <- dbOpen db dataSource userId password
                         if (not opened)
                          then finalize (dbDelete db)
                                        (dbRaiseExn db)
                          else return db)


-- | Closes a connection opened with 'dbConnect' (or 'dbConnectDirect').
dbDisconnect :: Db a -> IO ()
dbDisconnect db
  = do freed <- dbFreeConnection db
       if (freed) 
        then return ()
        else do dbClose db
                dbDelete db


{----------------------------------------------------------
  Database meta information
----------------------------------------------------------}
type DataSourceName = String
type TableName      = String

-- | Column names. Note that a column name consisting of a number can
-- be used to retrieve a value by index, for example: 'dbGetString' @db \"1\"'.
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
              , columnForeignKey       :: Int -- ^ Is this a foreign key column? 0 = no, 1 = first key, 2 = second key, etc. (not supported on all systems)
              , columnPrimaryKey        :: Int -- ^ Is this a primary key column? 0 = no, 1 = first key, 2 = second key, etc. (not supported on all systems)
              , columnForeignKeyTableName :: TableName  -- ^ Table that has this foreign key as a primary key.
              , columnPrimaryKeyTableNames :: [TableName]  -- ^ Tables that use this primary key as a foreign key.
              }


-- | Get the complete meta information of a data source. Takes the
-- data source name, a user id, and password as arguments.
--
-- > dbGetDataSourceInfo dsn userid password
-- >   = dbWithConnection dsn userId password dbGetInfo 
-- 
dbGetDataSourceInfo :: DataSourceName -> String -> String -> IO DbInfo
dbGetDataSourceInfo dataSource userId password
  = dbWithConnection dataSource userId password dbGetInfo

-- | Get the meta information of a table in a data source. Takes the
-- data source name, table name, a user id, and password as arguments.
dbGetDataSourceTableInfo :: DataSourceName -> TableName -> String -> String -> IO TableInfo
dbGetDataSourceTableInfo dataSource tableName userId password
  = dbWithConnection dataSource userId password (\db -> dbGetTableInfo db tableName)

-- | Get the meta information of a table in a database.
dbGetTableInfo :: Db a -> TableName -> IO TableInfo
dbGetTableInfo db name
  = do info <- dbGetInfo db
       case lookup name (zip (map tableName (dbTables info)) (dbTables info)) of
         Nothing    -> raiseDbInvalidTableName db name
         Just tinfo -> return tinfo

-- | Get the complete meta information of a database.
dbGetInfo :: Db a -> IO DbInfo
dbGetInfo db
  = bracket (dbGetCatalog db "")
            (dbInfDelete)
            (\dbInf ->do catalog  <- dbInfGetCatalogName dbInf
                         schema   <- dbInfGetSchemaName  dbInf
                         numTables<- dbInfGetNumTables   dbInf
                         tables   <- mapM (\idx -> do tableInf <- dbInfGetTableInf dbInf (idx-1)
                                                      dbTableInfGetInfo tableInf db) 
                                          [1..numTables]
                         return (DbInfo catalog schema tables))

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
                          decdigits numprecrad fk pk fkname (parseTables pkname) )
  where
    -- tables formatted as: "[name1][name2]...". Parser basically admits anything :-)
    parseTables []        = []  -- done
    parseTables ('[':xs)  = let (name,ys) = span (/=']') xs  -- take till close bracket
                            in name : parseTables ys
    parseTables (']':xs)  = parseTables xs    -- ignore ']'
    parseTables (' ':xs)  = parseTables xs    -- ignore ' '
    parseTables xs        = [xs]              -- should not happen: take rest as a single database name


{----------------------------------------------------------
  Data sources
----------------------------------------------------------}
-- | Returns the name and description of the data sources on the system.
dbGetDataSources :: IO [(DataSourceName,String)]
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


-- | Get the data source name of a database.
dbGetDataSourceName :: Db a -> IO DataSourceName
dbGetDataSourceName db
  = dbGetDatasourceName db

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
              , dbDataSource :: DataSourceName
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
       dataSource<- dbGetDataSourceName db
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
  = do dataSource <- dbGetDataSourceName db
       raiseDbError (DbError "Type mismatch" dataSource DB_ERR_TYPE_MISMATCH 0 "" )

-- | Raise a fetch null error
raiseDbFetchNull :: Db a -> IO b
raiseDbFetchNull db
  = do dataSource <- dbGetDataSourceName db
       raiseDbError (DbError "Unexpected NULL value" dataSource DB_ERR_FETCH_NULL 0 "")

-- | Raise an invalid column name error
raiseDbInvalidColumnName :: Db a -> ColumnName -> IO b
raiseDbInvalidColumnName db name
  = do dataSource <- dbGetDataSourceName db
       raiseDbError (DbError ("Invalid column name/index (" ++ name ++ ")") dataSource DB_ERR_INVALID_COLUMN_NAME 0 "")

-- | Raise an invalid table name error
raiseDbInvalidTableName :: Db a -> ColumnName -> IO b
raiseDbInvalidTableName db name
  = do dataSource <- dbGetDataSourceName db
       raiseDbError (DbError ("Invalid table name (" ++ name ++ ")") dataSource DB_ERR_INVALID_TABLE_NAME 0 "")

-- | Raise a connection error
raiseDbConnect :: DataSourceName -> IO a
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
  | DB_ERR_INVALID_TABLE_NAME  -- ^ Invalid (or unknown) table name
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

-- | Convert a table name to a format that can be used directly in SQL statements.
-- For example, this call can do case conversion and quoting.
toSqlTableName :: Db a -> TableName -> TableName
toSqlTableName db name
  = unsafePerformIO $ dbSQLTableName db name

-- | Convert a column name to a format that can be used directly in SQL statements.
-- For example, this call can do case conversion and quoting.
toSqlColumnName :: Db a -> ColumnName -> ColumnName
toSqlColumnName db name
  = unsafePerformIO $ dbSQLColumnName db name


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
    ,"pkey   : " ++ show (columnPrimaryKey info)
    ,"ptable : " ++ show (columnPrimaryKeyTableNames info)
    ,"fkey   : " ++ show (columnForeignKey info)
    ,"ftable : " ++ columnForeignKeyTableName info
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
