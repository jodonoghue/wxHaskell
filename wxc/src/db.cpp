#include "wrapper.h"

#if wxUSE_ODBC
# include "wx/db.h"
#else
class wxDb;
class wxDbColInf; 
class wxDbInf;
class wxDbTableInf;
typedef void* HENV;
#endif

/* testing */
// #define wxUSE_ODBC 0

/*-----------------------------------------------------------------------------
  We want to include the function signatures always -- even on 
  systems that don't support ODBC. This means that every function body is
  surrounded by #ifdef wxUSE_ODBC directives :-(
-----------------------------------------------------------------------------*/
#if defined(wxUSE_ODBC) && (wxUSE_ODBC==0)
# undef wxUSE_ODBC
#endif

#ifndef wxUSE_ODBC
# define wxDb            void
# define wxDbConnectInf  void
#endif
 
#if defined(wxUSE_UNICODE) && (wxUSE_UNICODE==0)
# undef wxUSE_UNICODE
#endif

/* used for easy marshalling in Haskell */
enum StandardSqlType {
  SqlUnknown
 ,SqlChar
 ,SqlNumeric
 ,SqlDecimal
 ,SqlInteger
 ,SqlSmallInt
 ,SqlFloat
 ,SqlReal
 ,SqlDouble
 ,SqlDate
 ,SqlTime
 ,SqlTimeStamp
 ,SqlVarChar
 ,SqlBit
 ,SqlBinary
 ,SqlVarBinary
 ,SqlBigInt
 ,SqlTinyInt
};


#ifndef SQL_DEFAULT
# define SQL_DEFAULT SQL_CHAR
#endif

extern "C" {



/*-----------------------------------------------------------------------------
  HENV, HDBC, HSTMT
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,Null_HENV)()  
{
#ifdef wxUSE_ODBC  
  return (void*)(SQL_NULL_HENV); 
#else
  return NULL;
#endif
}

EWXWEXPORT(void*,Null_HDBC)()  
{
#ifdef wxUSE_ODBC
  return (void*)(SQL_NULL_HDBC); 
#else
  return NULL;
#endif
}

EWXWEXPORT(void*,Null_HSTMT)() 
{ 
#ifdef wxUSE_ODBC
  return (void*)(SQL_NULL_HSTMT); 
#else
  return NULL;
#endif
}
  

/*-----------------------------------------------------------------------------
  DbConnectInf
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxDbConnectInf*,wxDbConnectInf_Create)(void* henv, wxString* dsn, wxString* userID, wxString* password, wxString* defaultDir, wxString* description, wxString* fileType)
{
#ifdef wxUSE_ODBC
  return new wxDbConnectInf( (HENV)(henv), *dsn, *userID, *password, *defaultDir, *description, *fileType);
#else
  return NULL;
#endif
}

EWXWEXPORT(void,wxDbConnectInf_AllocHenv)(wxDbConnectInf* self)
{
#ifdef wxUSE_ODBC
  self->AllocHenv();
#endif
}

EWXWEXPORT(void,wxDbConnectInf_FreeHenv)(wxDbConnectInf* self)
{
#ifdef wxUSE_ODBC
  self->FreeHenv();
#endif
}

EWXWEXPORT(void*,wxDbConnectInf_GetHenv)(wxDbConnectInf* self)
{
#ifdef wxUSE_ODBC
  return (void*)(self->GetHenv());
#else
  return NULL;
#endif
}


EWXWEXPORT(void,wxDbConnectInf_Delete)(wxDbConnectInf* self)
{
#ifdef wxUSE_ODBC
  if (self != NULL) delete self;
#endif
}


/*-----------------------------------------------------------------------------
  Global functions
-----------------------------------------------------------------------------*/
EWXWEXPORT(int,wxDb_IsSupported)()
{
#ifdef wxUSE_ODBC
  return true;
#else
  return false;
#endif
}

EWXWEXPORT(void,wxDb_CloseConnections)()
{
#ifdef wxUSE_ODBC
  wxDbCloseConnections();
#endif
}

EWXWEXPORT(int,wxDb_ConnectionsInUse)()
{
#ifdef wxUSE_ODBC
  return wxDbConnectionsInUse();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxDb_FreeConnection)( wxDb* db)
{
#ifdef wxUSE_ODBC
  return wxDbFreeConnection(db);
#else
  return false;
#endif
}

EWXWEXPORT(wxDb*,wxDb_GetConnection)( wxDbConnectInf* connectInf, bool fwdCursorsOnly )
{
#ifdef wxUSE_ODBC
  return wxDbGetConnection(connectInf,fwdCursorsOnly);
#else
  return NULL;
#endif
}

EWXWEXPORT(int,wxDb_GetDataSource)( void* henv, wxChar* dsn, int dsnLen, char* description, int descLen, int direction)
{
  if (dsn && dsnLen > 0)   *dsn = '\0';
  if (description && descLen > 0) *description = '\0';
#ifdef wxUSE_ODBC
  return wxDbGetDataSource( (HENV)(henv), dsn, dsnLen, description, descLen, direction);
#else
  return 0;
#endif
}


/* translate a system sql type to standard range of enumerations */
EWXWEXPORT(int,wxDb_SqlTypeToStandardSqlType)( int sqlType )
{
#ifdef wxUSE_ODBC
  switch (sqlType) {
    case SQL_CHAR:      return SqlChar;
    case SQL_NUMERIC:   return SqlNumeric;
    case SQL_DECIMAL:   return SqlDecimal;
    case SQL_INTEGER:   return SqlInteger;
    case SQL_SMALLINT:  return SqlSmallInt;
    case SQL_FLOAT:     return SqlFloat;
    case SQL_REAL:      return SqlReal;
    case SQL_DOUBLE:    return SqlDouble;
#ifdef SQL_DATE
    case SQL_DATE:      return SqlDate;
    case SQL_TIME:      return SqlTime;
    case SQL_TIMESTAMP: return SqlTimeStamp;
#endif
    case SQL_VARCHAR:   return SqlVarChar;
#ifdef SQL_BIT
    case SQL_BIT:       return SqlBit;
#endif
#ifdef SQL_BINARY
    case SQL_BINARY:    return SqlBinary;
    case SQL_VARBINARY: return SqlVarBinary;
#endif
#ifdef SQL_BIGINT
    case SQL_BIGINT:    return SqlBigInt;
    case SQL_TINYINT:   return SqlTinyInt;
#endif
    default:  return SqlUnknown;
  }
#else
  return SqlUnknown;
#endif
}

EWXWEXPORT(int,wxDb_StandardSqlTypeToSqlType)( int sqlType )
{
#ifdef wxUSE_ODBC
  switch (sqlType) {
    case SqlChar      : return SQL_CHAR;
    case SqlNumeric   : return SQL_NUMERIC;
    case SqlDecimal   : return SQL_DECIMAL;
    case SqlInteger   : return SQL_INTEGER;
    case SqlSmallInt  : return SQL_SMALLINT;
    case SqlFloat     : return SQL_FLOAT;
    case SqlReal      : return SQL_REAL;
    case SqlDouble    : return SQL_DOUBLE;
#ifdef SQL_DATE
    case SqlDate      : return SQL_DATE;
    case SqlTime      : return SQL_TIME;
    case SqlTimeStamp : return SQL_TIMESTAMP;
#endif
    case SqlVarChar   : return SQL_VARCHAR;
#ifdef SQL_BIT
    case SqlBit       : return SQL_BIT;
#endif
#ifdef SQL_BINARY
    case SqlBinary    : return SQL_BINARY;
    case SqlVarBinary : return SQL_VARBINARY;
#endif
#ifdef SQL_BIGINT
    case SqlBigInt    : return SQL_BIGINT;
    case SqlTinyInt   : return SQL_TINYINT;
#endif
    case SqlUnknown   : return SQL_DEFAULT;
    default           : return SQL_DEFAULT;
  }
#else
  return 0;
#endif
}


/*-----------------------------------------------------------------------------
  Db
-----------------------------------------------------------------------------*/
EWXWEXPORT(int,wxDb_GetStatus)(wxDb* db)
{
#ifdef wxUSE_ODBC
  if (db==NULL)
    return DB_ERR_GENERAL_ERROR;
  else
    return db->DB_STATUS;
#else
  return (-1);
#endif
}

EWXWEXPORT(wxString*,wxDb_GetErrorMsg)(wxDb* db)
{
#ifdef wxUSE_ODBC
  if (db==NULL)
    return new wxString(wxT("unconnected database (is NULL)"));
  else
    return new wxString(db->errorMsg,db->cbErrorMsg);
#else
  return new wxString(wxT("ODBC is not supported on this platform (recompile with ODBC support)"));
#endif
}

EWXWEXPORT(int,wxDb_GetNumErrorMessages)( wxDb* db)
{
#ifdef wxUSE_ODBC
  int n;
  /* find the last non-empty entry */
  for( n = DB_MAX_ERROR_HISTORY; n > 0; n-- ) {
    if (db->errorList[n-1] != NULL && db->errorList[n-1][0] != 0) return n;
  }
  /* hmm, nothing found, return the last non-null entry instead */
  for( n = DB_MAX_ERROR_HISTORY; n > 0; n-- ) {
    if (db->errorList[n-1] != NULL) return n;
  } 
  return 0;
#else
  return 0;
#endif
}

EWXWEXPORT(wxString*,wxDb_GetErrorMessage)(wxDb* db, int index)
{
#ifdef wxUSE_ODBC
  int n,idx;
  if (db==NULL) return new wxString(wxT(""));
  n = wxDb_GetNumErrorMessages(db);
  if (index >= n) index = n-1;
  if (index < 0)  index = 0;  
  idx = n - index - 1;
  if (idx < 0 || idx >= DB_MAX_ERROR_HISTORY)
    return new wxString(wxT("unknown error"));
  else
    return new wxString(db->errorList[n - index - 1]);
#else
  return new wxString(wxT("ODBC is not supported on this platform (recompile with ODBC support)"));
#endif
}

EWXWEXPORT(int,wxDb_GetNativeError)(wxDb* db)
{
#ifdef wxUSE_ODBC
  if (db==NULL)
    return (-1);
  else
    return db->nativeError;
#else
  return (-1);
#endif
}


EWXWEXPORT(int,wxDb_IsOpen)(wxDb* db)
{
#ifdef wxUSE_ODBC
  if (db==NULL)
    return false;
  else
    return db->IsOpen();
#else
  return false;
#endif
}


EWXWEXPORT(void,wxDb_Close)(wxDb* db)
{
#ifdef wxUSE_ODBC
  db->Close();
#endif
}

EWXWEXPORT(int,wxDb_CommitTrans)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->CommitTrans();
#else
  return false;
#endif
}

EWXWEXPORT(int,wxDb_RollbackTrans)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->RollbackTrans();
#else
  return false;
#endif
}

EWXWEXPORT(void*,wxDb_GetHENV)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->GetHENV();
#else
  return Null_HENV();
#endif
}

EWXWEXPORT(void*,wxDb_GetHDBC)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->GetHDBC();
#else
  return Null_HDBC();
#endif
}

EWXWEXPORT(void*,wxDb_GetHSTMT)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->GetHSTMT();
#else
  return Null_HSTMT();
#endif
}


EWXWEXPORT(int,wxDb_GetNextError)(wxDb* db, void* henv, void* hdbc, void* hstmt)
{
#ifdef wxUSE_ODBC
  return db->GetNextError( (HENV)(henv), (HDBC)(hdbc), (HSTMT)(hstmt) );
#else
  return false;
#endif
}

EWXWEXPORT(int,wxDb_ExecSql)(wxDb* db, wxString* sql)
{
#ifdef wxUSE_ODBC
  return db->ExecSql(*sql);
#else
  return false;
#endif
}

EWXWEXPORT(int,wxDb_GetNext)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->GetNext();
#else
  return false;
#endif
}


EWXWEXPORT(int,wxDb_GetData)(wxDb* db, int column, int ctype, void* data, int dataLen, int* usedLen ) 
{
#ifdef wxUSE_ODBC
  long used;
  bool result = db->GetData( column, ctype, data, dataLen, &used );
  if (usedLen) *usedLen = used;
  return result;
#else
  if (usedLen) *usedLen = 0;
  return false;
#endif
}

EWXWEXPORT(int,wxDb_GetDataInt)(wxDb* db, int column, int* i, int* usedLen ) 
{
#ifdef wxUSE_ODBC
  long used;
  long value  = 0;
  bool result = db->GetData( column, SQL_C_LONG, &value, sizeof(value), &used );
  if (usedLen) *usedLen = used;
  if (i) *i = value;
  return result;
#else
  return false;
#endif
}

EWXWEXPORT(int,wxDb_GetDataDouble)(wxDb* db, int column, double* d, int* usedLen ) 
{
#ifdef wxUSE_ODBC
  long used;
  double value  = 0;
  bool result = db->GetData( column, SQL_C_DOUBLE, &value, sizeof(value), &used );
  if (usedLen) *usedLen = used;
  if (d) *d = value;
  return result;
#else
  return false;
#endif
}


EWXWEXPORT(int,wxDb_GetDataBinary)(wxDb* db, int column, bool asChars, char** pbuf, int* plen ) 
{
  /* DAAN: A rather complicated procedure to retrieve SQL data as a string or
     binary value. Since there is no trustworthy way to retrieve the size of
     the returned data beforehand, we start with a static buffer that is at
     least large enough to hold all basic data types. We than allocate the
     correct size in the heap, or when the data buffer was too small, we
     repetively call GetData to retrieve the rest of the data.
  */
#ifdef wxUSE_ODBC
#define STATICBUF_LEN 512
  long  needed   = 0;
  bool  ok       = false;
  int   sqlType  = (asChars ? SQL_C_CHAR : SQL_C_BINARY);  
  char  staticBuf[STATICBUF_LEN+1];

  /* init args */
  *pbuf = NULL;
  *plen = 0;    

  /* get data */
  ok = db->GetData( column, sqlType, staticBuf, STATICBUF_LEN, &needed );
  
  /* case 1: no error and data is in 'staticBuf' */    
  if (ok) {
    if (needed > 0 && needed != SQL_NULL_DATA) {
      *pbuf = (char*)malloc( needed );
      if (*pbuf==NULL) return false;
      memcpy( *pbuf, staticBuf, needed );
    }
    *plen = needed;
    return ok;
  }
  
  /* case 2: buffer was too small, repetitively read data */
  else if (db->DB_STATUS == DB_ERR_DATA_TRUNCATED)
  {
    char* buf      = NULL;
    long  bufLen   = 0;
    long  totalLen = (asChars ? STATICBUF_LEN-1 : STATICBUF_LEN); /* less the terminating 0 ? */
    
    /* try to allocate correct length */
    if (needed > totalLen && needed != SQL_NO_TOTAL) 
      bufLen = needed + 16; /* DAAN: somehow, we need at least 2 more items to prevent reallocs */
    else
      bufLen = 2*STATICBUF_LEN;

    buf = (char*)malloc( bufLen );
    if (buf==NULL) return false;
    memcpy( buf, staticBuf, totalLen );
    
    ok = db->GetData( column, sqlType, buf+totalLen, bufLen-totalLen, &needed ); 
    while (!ok && db->DB_STATUS == DB_ERR_DATA_TRUNCATED)
    {
      char* newbuf;
      totalLen = (asChars ? bufLen-1 : bufLen);  /* less the terminating 0 ? */
    
      /* realloc the buffer exponentially */
      newbuf = (char*)realloc( buf, 2*bufLen );
      if (newbuf == NULL) {
        free(buf);
        return false;
      }
      buf    = newbuf;
      bufLen = 2*bufLen;

      /* add more data */
      ok = db->GetData( column, sqlType, buf+totalLen, bufLen-totalLen, &needed );
    }
    
    /* check for errors */
    if (!ok || needed == SQL_NO_TOTAL) {
      free(buf);
      return false;
    }
    
    /* add the last added data length to the total length */
    totalLen += needed;

    /* set results */
    *pbuf = buf;
    *plen = totalLen;
    return ok;
  }
  
  /* case 3: real error */
  else {
    *plen = needed; /* could contain error code */
    return ok;
  }
#else
  if (pbuf) *pbuf = NULL;
  if (plen) *plen = 0;
  return false;
#endif
}


EWXWEXPORT(int,wxDb_GetDataDate)(wxDb* db, int column, int* ctime, int* usedLen ) 
{
#if defined(wxUSE_ODBC) && defined(SQL_C_TYPE_DATE)
  SQL_DATE_STRUCT date;
  long used;
  bool ok;

  if (ctime) *ctime = -1;
  ok = db->GetData( column, SQL_C_TYPE_DATE, &date, sizeof(date), &used );
  if (usedLen) *usedLen = used;
  if (ok && used != SQL_NULL_DATA) {
    struct tm datetm;
    time_t    secs;
    memset(&datetm,0,sizeof(datetm));
    datetm.tm_year  = date.year - 1900;
    datetm.tm_mon   = date.month - 1;
    datetm.tm_mday  = date.day;
    datetm.tm_isdst = -1;
    secs = mktime(&datetm);
    if (ctime) *ctime = secs;
  }
  return ok;
#else
  return false;
#endif
}

EWXWEXPORT(int,wxDb_GetDataTimeStamp)(wxDb* db, int column, int* ctime, int* fraction, int* usedLen ) 
{
#if defined(wxUSE_ODBC) && defined(SQL_C_TYPE_TIMESTAMP)
  SQL_TIMESTAMP_STRUCT date;
  long used;
  bool ok;

  if (ctime)    *ctime = -1;
  if (fraction) *fraction = -1;

  ok = db->GetData( column, SQL_C_TYPE_TIMESTAMP, &date, sizeof(date), &used );
  if (usedLen) *usedLen = used;
  if (ok && used != SQL_NULL_DATA) {
    struct tm datetm;
    time_t    secs;
    memset(&datetm,0,sizeof(datetm));
    datetm.tm_year  = date.year - 1900;
    datetm.tm_mon   = date.month - 1;
    datetm.tm_mday  = date.day;
    datetm.tm_hour  = date.hour;
    datetm.tm_min   = date.minute;
    datetm.tm_sec   = date.second;
    datetm.tm_isdst = -1;
    secs = mktime(&datetm);
    if (ctime)    *ctime = secs;
    if (fraction) *fraction = date.fraction;
  }
  return ok;
#else
  return false;
#endif
}

EWXWEXPORT(int,wxDb_GetDataTime)(wxDb* db, int column, int* secs, int* usedLen ) 
{
#if defined(wxUSE_ODBC) && defined(SQL_C_TYPE_TIME)
  SQL_TIME_STRUCT sqltime;
  long used;
  bool ok;

  if (secs)    *secs = -1;
  ok = db->GetData( column, SQL_C_TYPE_TIME, &sqltime, sizeof(sqltime), &used );
  if (usedLen) *usedLen = used;
  if (ok && used != SQL_NULL_DATA) {
    if (secs)  *secs = 3600*sqltime.hour + 60*sqltime.minute + sqltime.second;
  }
  return ok;
#else
  return false;
#endif
}

EWXWEXPORT(int,wxDb_Dbms)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->Dbms();
#else
  return 0;
#endif
}

EWXWEXPORT(wxString*,wxDb_GetDatabaseName)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return new wxString(db->GetDatabaseName());
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDb_GetDatasourceName)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return new wxString(db->GetDatasourceName());
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDb_GetPassword)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return new wxString(db->GetPassword());
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDb_GetUsername)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return new wxString(db->GetUsername());
#else
  return NULL;
#endif
}

EWXWEXPORT(int,wxDb_Grant)(wxDb* db, int privileges, wxString* tableName, wxString* userList )
{
#ifdef wxUSE_ODBC
  return db->Grant(privileges, *tableName, *userList);
#else
  return false;
#endif
}

EWXWEXPORT(int,wxDb_GetTableCount)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->GetTableCount();
#else
  return 0;
#endif
}
EWXWEXPORT(wxDbInf*,wxDb_GetCatalog)( wxDb* db, wxString* userName )
{
#ifdef wxUSE_ODBC
  if (userName!=NULL && userName->IsEmpty()) {
    userName=NULL;
  }
  return db->GetCatalog((userName ? userName->c_str() : NULL));
#else
  return NULL;
#endif
}

EWXWEXPORT(int,wxDb_GetColumnCount)(wxDb* db, wxString* tableName, wxString* userName )
{
#ifdef wxUSE_ODBC
  if (userName!=NULL && userName->IsEmpty()) {
    userName=NULL;
  }
  return db->GetColumnCount((userName ? userName->c_str() : NULL));
#else
  return 0;
#endif
}

EWXWEXPORT(wxDbColInf*,wxDb_GetColumns)(wxDb* db, wxString* tableName, int* columnCount, wxString* userName)
{
#ifdef wxUSE_ODBC
  UWORD       count  = 0;
  wxDbColInf* result = NULL;
  if (userName!=NULL && userName->IsEmpty()) {
    userName=NULL;
  }
  result = db->GetColumns(*tableName,&count,(userName ? userName->c_str() : NULL));
  if (columnCount) *columnCount = count;
  return result;
#else
  return NULL;
#endif
}

EWXWEXPORT(int,wxDb_Open)(wxDb* db, wxString* dsn, wxString* userId, wxString* password)
{
#ifdef wxUSE_ODBC
  return db->Open(*dsn,*userId,*password);
#else
  return false;
#endif
}

EWXWEXPORT(wxString*,wxDb_SQLColumnName)(wxDb* db, wxString* columnName)
{
#ifdef wxUSE_ODBC
  return new wxString(db->SQLColumnName(*columnName));
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDb_SQLTableName)(wxDb* db, wxString* tableName)
{
#ifdef wxUSE_ODBC
  return new wxString(db->SQLTableName(*tableName));
#else
  return NULL;
#endif
}


EWXWEXPORT(int,wxDb_TableExists)(wxDb* db, wxString* tableName, wxString* userName, wxString* path )
{
#ifdef wxUSE_ODBC
  if (userName!=NULL && userName->IsEmpty()) {
    userName=NULL;
  }
  return db->TableExists(*tableName,(userName ? userName->c_str() : NULL),*path);
#else
  return false;
#endif
}


EWXWEXPORT(int,wxDb_TablePrivileges)(wxDb* db, wxString* tableName, wxString* privileges, wxString* userName, wxString* schema, wxString* path )
{
#ifdef wxUSE_ODBC
  if (schema!=NULL && schema->IsEmpty()) {
    schema=NULL;
  }
  return db->TablePrivileges(*tableName,*privileges,(userName==NULL ? wxT("") : userName->c_str()),(schema ? schema->c_str() : NULL),*path);
#else
  return false;
#endif
}


EWXWEXPORT(int,wxDb_TranslateSqlState)(wxDb* db, wxString* sqlState)
{
#ifdef wxUSE_ODBC
  return db->TranslateSqlState(*sqlState);
#else
  return 0;
#endif
}


EWXWEXPORT(wxDb*,wxDb_Create)( HENV henv, bool fwdOnlyCursors )
{
#ifdef wxUSE_ODBC
  return new wxDb(henv,fwdOnlyCursors);
#else
  return NULL;
#endif
}

EWXWEXPORT(void,wxDb_Delete)( wxDb* db )
{
#ifdef wxUSE_ODBC
  if (db!=NULL) delete db;
#endif
}

/*-----------------------------------------------------------------------------
  DbInf
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxString*,wxDbInf_GetCatalogName)( wxDbInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->catalog);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbInf_GetSchemaName)( wxDbInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->schema);
#else
  return NULL;
#endif
}

EWXWEXPORT(int,wxDbInf_GetNumTables)( wxDbInf* self )
{
#ifdef wxUSE_ODBC
  return (self->numTables);
#else
  return 0;
#endif
}

EWXWEXPORT(wxDbTableInf*,wxDbInf_GetTableInf)( wxDbInf* self, int index )
{
#ifdef wxUSE_ODBC
  if (self && self->pTableInf && index >= 0 && index < self->numTables)
    return &self->pTableInf[index];
  else
    return NULL;
#else
  return NULL;
#endif
}

EWXWEXPORT(void,wxDbInf_Delete)( wxDbInf* self )
{
#ifdef wxUSE_ODBC
  if (self!=NULL) delete self;
#endif
}

/*-----------------------------------------------------------------------------
  DbTableInf
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxString*,wxDbTableInf_GetTableName)( wxDbTableInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->tableName);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbTableInf_GetTableType)( wxDbTableInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->tableType);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbTableInf_GetTableRemarks)( wxDbTableInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->tableRemarks);
#else
  return NULL;
#endif
}

EWXWEXPORT(int,wxDbTableInf_GetNumCols)( wxDbTableInf* self )
{
#ifdef wxUSE_ODBC
  return (self->numCols);
#else
  return 0;
#endif
}

/*-----------------------------------------------------------------------------
  DbColInfArray
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxDbColInf*,wxDbColInfArray_GetColInf)( wxDbColInf* self, int index )
{
#ifdef wxUSE_ODBC
  return &self[index];
#else
  return NULL;
#endif
}

EWXWEXPORT(void,wxDbColInfArray_Delete)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  if (self!=NULL) delete [] self;
#endif
}

/*-----------------------------------------------------------------------------
  DbColInf
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxString*,wxDbColInf_GetCatalog)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->catalog);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbColInf_GetSchema)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->schema);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbColInf_GetTableName)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->tableName);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbColInf_GetColName)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->colName);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbColInf_GetTypeName)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->typeName);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbColInf_GetRemarks)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->remarks);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbColInf_GetPkTableName)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->PkTableName);
#else
  return NULL;
#endif
}

EWXWEXPORT(wxString*,wxDbColInf_GetFkTableName)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return new wxString(self->FkTableName);
#else
  return NULL;
#endif
}

EWXWEXPORT(int,wxDbColInf_GetSqlDataType)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return self->sqlDataType;
#else
  return (-1);
#endif
}

EWXWEXPORT(int,wxDbColInf_GetColumnSize)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return self->columnSize;
#else
  return (-1);
#endif
}

EWXWEXPORT(int,wxDbColInf_GetBufferLength)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return self->bufferLength;
#else
  return (-1);
#endif
}

EWXWEXPORT(int,wxDbColInf_GetDecimalDigits)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return self->decimalDigits;
#else
  return (-1);
#endif
}

EWXWEXPORT(int,wxDbColInf_GetNumPrecRadix)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return self->numPrecRadix;
#else
  return (-1);
#endif
}

EWXWEXPORT(int,wxDbColInf_GetDbDataType)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return self->dbDataType;
#else
  return (-1);
#endif
}

EWXWEXPORT(int,wxDbColInf_GetPkCol)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return self->PkCol;
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxDbColInf_GetFkCol)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return self->FkCol;
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxDbColInf_IsNullable)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return (self->nullable != 0);
#else
  return false;
#endif
}

/*-----------------------------------------------------------------------------
  Result set information.
  Unfortunately, the wxWindows standard interface doesn't have a way
  to retrieve column information for a query result -- so we add it
  here ourselves.
-----------------------------------------------------------------------------*/

EWXWEXPORT(wxDbColInf*, wxDb_GetResultColumns)( wxDb* db, int* pnumCols )
{
#ifndef wxUSE_ODBC
  if (pnumCols) *pnumCols = 0;
  return NULL;
#else
  RETCODE retcode = 0;
  HSTMT   hstmt   = SQL_NULL_HSTMT;
  SWORD   numCols = 0;
  
  UWORD       column = 0;
  wxDbColInf* colInf = NULL;

  if (pnumCols) *pnumCols = 0;
  if (db==NULL) return NULL;

  /* allocate column info's */
  hstmt   = db->GetHSTMT();
  retcode = SQLNumResultCols(hstmt,&numCols);
  if (retcode != SQL_SUCCESS) {
    db->DispAllErrors(db->GetHENV(), db->GetHDBC(), hstmt);
    return NULL;
  }
  if (numCols==0) return NULL; 
  
  colInf = new wxDbColInf[numCols+1];
  if (!colInf) return NULL;

  /* mark the end of the array */
  wxStrcpy(colInf[numCols].tableName, wxEmptyString);
  wxStrcpy(colInf[numCols].colName, wxEmptyString);
  colInf[numCols].sqlDataType = 0;

  /* initialize all column infos */
  for( column = 0; column < numCols; column++)
  {
    SWORD  colNameLen  = 0;
    SWORD  typeNameLen = 0;
    UDWORD colSize     = 0;

    /* get the column information */
    retcode = SQLDescribeCol(hstmt, column+1,
#ifndef wxUSE_UNICODE
                            (SQLCHAR*) colInf[column].colName,
#else
                            (SQLWCHAR*) colInf[column].colName,
#endif
                            DB_MAX_COLUMN_NAME_LEN+1, &colNameLen,
                            &colInf[column].sqlDataType,
                            &colSize,
                            &colInf[column].decimalDigits, 
                            &colInf[column].nullable );
    colInf[column].columnSize   = colSize;
    
    /* check for errors */
    if (retcode != SQL_SUCCESS) {
      db->DispAllErrors(db->GetHENV(), db->GetHDBC(), hstmt);
      delete [] colInf;
      return NULL;
    }

#ifdef SQL_DESC_TYPE_NAME    
    /* try to get type name too (errors are no problem) */
    SQLColAttribute( hstmt, column+1, SQL_DESC_TYPE_NAME,
                     colInf[column].typeName, 128+1,
                     &typeNameLen, NULL );
#endif

    /* for compatibilty with the wxWindows GetColumns, we set the dbDataType too */
    colInf[column].dbDataType = 0;
    switch (colInf[column].sqlDataType)
    {
        case SQL_VARCHAR:
        case SQL_CHAR:
            colInf[column].dbDataType = DB_DATA_TYPE_VARCHAR;
        break;

        case SQL_TINYINT:
        case SQL_SMALLINT:
        case SQL_INTEGER:
#ifdef SQL_BIGINT
        case SQL_BIGINT:
#endif
#ifdef SQL_BIT
        case SQL_BIT:
#endif
            colInf[column].dbDataType = DB_DATA_TYPE_INTEGER;
            break;
        case SQL_DOUBLE:
        case SQL_DECIMAL:
        case SQL_NUMERIC:
        case SQL_FLOAT:
        case SQL_REAL:
            colInf[column].dbDataType = DB_DATA_TYPE_FLOAT;
            break;
#ifdef SQL_DATE
        case SQL_DATE:
            colInf[column].dbDataType = DB_DATA_TYPE_DATE;
            break;
#endif
#ifdef SQL_BINARY
        case SQL_BINARY:
            colInf[column].dbDataType = DB_DATA_TYPE_BLOB;
            break;
#endif
#ifdef __WXDEBUG__
        default:
            wxString errMsg;
            errMsg.Printf(wxT("SQL Data type %d currently not supported by wxWindows"), colInf[column].sqlDataType);
            wxLogDebug(errMsg,wxT("ODBC DEBUG MESSAGE"));
#endif
    }
  } /* for columns */

  if (pnumCols) *pnumCols = numCols;
  return colInf;
#endif
}

}
