#include "wrapper.h"
#include "wx/db.h"


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
EWXWEXPORT(bool,wxDb_IsSupported)()
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

EWXWEXPORT(bool,wxDb_FreeConnection)( wxDb* db)
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

EWXWEXPORT(bool,wxDb_GetDataSource)( void* henv, char* dsn, int dsnLen, char* description, int descLen, int direction)
{
  if (dsn && dsnLen > 0)   *dsn = '\0';
  if (description && descLen > 0) *description = '\0';
#ifdef wxUSE_ODBC
  return wxDbGetDataSource( (HENV)(henv), dsn, dsnLen, description, descLen, direction);
#else
  return NULL;
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
    case SqlUnknown   : return SQL_DEFAULT;
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
    return new wxString("unconnected database (is NULL)");
  else
    return new wxString(db->errorMsg,db->cbErrorMsg);
#else
  return new wxString("ODBC is not supported on this platform");
#endif
}

EWXWEXPORT(wxString*,wxDb_GetErrorMessage)(wxDb* db, int index)
{
#ifdef wxUSE_ODBC
  if (db==NULL)
    return new wxString("");
  else
    return new wxString(db->errorList[index]);
#else
  return (-1);
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


EWXWEXPORT(bool,wxDb_IsOpen)(wxDb* db)
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

EWXWEXPORT(bool,wxDb_CommitTrans)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->CommitTrans();
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxDb_RollbackTrans)(wxDb* db)
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


EWXWEXPORT(bool,wxDb_GetNextError)(wxDb* db, void* henv, void* hdbc, void* hstmt)
{
#ifdef wxUSE_ODBC
  return db->GetNextError( (HENV)(henv), (HDBC)(hdbc), (HSTMT)(hstmt) );
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxDb_ExecSql)(wxDb* db, wxString* sql)
{
#ifdef wxUSE_ODBC
  return db->ExecSql(*sql);
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxDb_GetNext)(wxDb* db)
{
#ifdef wxUSE_ODBC
  return db->GetNext();
#else
  return false;
#endif
}


EWXWEXPORT(bool,wxDb_GetData)(wxDb* db, int column, int ctype, void* data, int dataLen, int* usedLen ) 
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

EWXWEXPORT(bool,wxDb_GetDataInt)(wxDb* db, int column, int* i, int* usedLen ) 
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

EWXWEXPORT(bool,wxDb_GetDataDouble)(wxDb* db, int column, double* d, int* usedLen ) 
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

EWXWEXPORT(bool,wxDb_GetDataString)(wxDb* db, int column, char* buf, int bufLen, int* usedLen ) 
{
#ifdef wxUSE_ODBC
  long used;
  bool result = db->GetData( column, SQL_C_CHAR, buf, bufLen, &used );
  if (usedLen) *usedLen = used;
  return result;
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

EWXWEXPORT(bool,wxDb_Grant)(wxDb* db, int privileges, wxString* tableName, wxString* userList )
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
  USHORT      count  = 0;
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

EWXWEXPORT(bool,wxDb_Open)(wxDb* db, wxString* dsn, wxString* userId, wxString* password)
{
#ifdef wxUSE_ODBC
  return db->Open(*dsn,*userId,*password);
#else
  return NULL;
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


EWXWEXPORT(bool,wxDb_TableExists)(wxDb* db, wxString* tableName, wxString* userName, wxString* path )
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


EWXWEXPORT(bool,wxDb_TablePrivileges)(wxDb* db, wxString* tableName, wxString* privileges, wxString* userName, wxString* schema, wxString* path )
{
#ifdef wxUSE_ODBC
  if (schema!=NULL && schema->IsEmpty()) {
    schema=NULL;
  }
  return db->TablePrivileges(*tableName,*privileges,(userName==NULL ? "" : userName->c_str()),(schema ? schema->c_str() : NULL),*path);
#else
  return false;
#endif
}


EWXWEXPORT(int,wxDb_TranslateSqlState)(wxDb* db, wxString* sqlState)
{
#ifdef wxUSE_ODBC
  return db->TranslateSqlState(*sqlState);
#else
  return NULL;
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
  return (-1);
#endif
}

EWXWEXPORT(int,wxDbColInf_GetFkCol)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return self->FkCol;
#else
  return (-1);
#endif
}

EWXWEXPORT(bool,wxDbColInf_IsNullable)( wxDbColInf* self )
{
#ifdef wxUSE_ODBC
  return (self->nullable != 0);
#else
  return false;
#endif
}

}