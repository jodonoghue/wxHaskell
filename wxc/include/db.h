/*-----------------------------------------------------------------------------
  HENV, HDBC, HSTMT
-----------------------------------------------------------------------------*/
TClassDef(wxHENV);
TClassDef(wxHDBC);
TClassDef(wxHSTMT);

TClass(wxHENV)  Null_HENV();
TClass(wxHDBC)  Null_HDBC();
TClass(wxHSTMT) Null_HSTMT();

/*-----------------------------------------------------------------------------
  Global
-----------------------------------------------------------------------------*/
TBool wxDb_IsSupported();
int   wxDb_SqlTypeToStandardSqlType( int sqlType );
int   wxDb_StandardSqlTypeToSqlType( int sqlType );
void  wxDb_CloseConnections();
int   wxDb_ConnectionsInUse();
TClass(wxDb)  wxDb_GetConnection( TClass(wxDbConnectInf) connectInf, TBool fwdCursorsOnly );
TBool wxDb_FreeConnection( TClass(wxDb) db);
TBool wxDb_GetDataSource( TClass(HENV) henv, void* dsn, int dsnLen, void* description, int descLen, int direction );

/*-----------------------------------------------------------------------------
  ConnectInf
-----------------------------------------------------------------------------*/
TClassDef(wxDbConnectInf);

TClass(wxDbConnectInf) wxDbConnectInf_Create(TClass(wxHENV) henv, TClass(wxString) dsn, TClass(wxString) userID, TClass(wxString) password, TClass(wxString) defaultDir, TClass(wxString) description, TClass(wxString) fileType);
void wxDbConnectInf_Delete(TSelf(wxDbConnectInf) self);
void wxDbConnectInf_AllocHenv(TSelf(wxDbConnectInf) self);
void wxDbConnectInf_FreeHenv(TSelf(wxDbConnectInf) self);
TClass(wxHENV) wxDbConnectInf_GetHenv(TSelf(wxDbConnectInf) self);

/*-----------------------------------------------------------------------------
  Db
-----------------------------------------------------------------------------*/
TClassDef(wxDb);

int              wxDb_GetStatus(TSelf(wxDb) db);
int              wxDb_GetNativeError( TSelf(wxDb) db);
/** Retrieve error message set by 'dbGetNextError' */
TClass(wxString) wxDb_GetErrorMsg(TSelf(wxDb) db);
/** Retrieve the last /n/ error messages, where
    /n/ is 'dbGetNumErrorMessages'. Index 0 is the most recent error
   that corresponds with 'dbGetStatus' and 'dbGetNativeError' */
TClass(wxString) wxDb_GetErrorMessage( TSelf(wxDb) db, int index);
/** Get the number of stored error messages. */
int wxDb_GetNumErrorMessages( TSelf(wxDb) db);
TBool wxDb_IsOpen(TSelf(wxDb) db );
void wxDb_Close(TSelf(wxDb) db);
TBool wxDb_CommitTrans(TSelf(wxDb) db);
TBool wxDb_RollbackTrans(TSelf(wxDb) db);
TClass(wxHENV) wxDb_GetHENV(TSelf(wxDb) db);
TClass(wxHDBC) wxDb_GetHDBC(TSelf(wxDb) db);
TClass(wxHSTMT) wxDb_GetHSTMT(TSelf(wxDb) db);
TBool wxDb_GetNextError(TSelf(wxDb) db, TClass(wxHENV) henv, TClass(wxHDBC) hdbc, TClass(wxHSTMT) hstmt);
TBool wxDb_ExecSql(TSelf(wxDb) db, TClass(wxString) sql);
TBool wxDb_GetNext(TSelf(wxDb) db);
TBool wxDb_GetData(TSelf(wxDb) db, int column, int ctype, void* data, int dataLen, int* usedLen );
TBool wxDb_GetDataInt(TSelf(wxDb) db, int column, int* i, int* usedLen );
TBool wxDb_GetDataDouble(TSelf(wxDb) db, int column, double* d, int* usedLen );
TBool wxDb_GetDataString(TSelf(wxDb) db, int column, void* buf, int bufLen, int* usedLen );

int wxDb_Dbms(TSelf(wxDb) db);
TClass(wxString) wxDb_GetDatabaseName(TSelf(wxDb) db);
TClass(wxString) wxDb_GetDatasourceName(TSelf(wxDb) db);
TClass(wxString) wxDb_GetPassword(TSelf(wxDb) db);
TClass(wxString) wxDb_GetUsername(TSelf(wxDb) db);
TBool wxDb_Grant(TSelf(wxDb) db, int privileges, TClass(wxString) tableName, TClass(wxString) userList );
int wxDb_GetTableCount(TSelf(wxDb) db);
TClass(wxDbInf) wxDb_GetCatalog( TSelf(wxDb) db, TClass(wxString) userName );
int wxDb_GetColumnCount(TSelf(wxDb) db, TClass(wxString) tableName, TClass(wxString) userName );
TClass(wxDbColInfArray) wxDb_GetColumns(TSelf(wxDb) db, TClass(wxString) tableName, int* columnCount, TClass(wxString) userName);
TBool wxDb_Open(TSelf(wxDb) db, TClass(wxString) dsn, TClass(wxString) userId, TClass(wxString) password);
TClass(wxString) wxDb_SQLColumnName(TSelf(wxDb) db, TClass(wxString) columnName);
TClass(wxString) wxDb_SQLTableName(TSelf(wxDb) db, TClass(wxString) tableName);
TBool wxDb_TableExists(TSelf(wxDb) db, TClass(wxString) tableName, TClass(wxString) userName, TClass(wxString) path );
TBool wxDb_TablePrivileges(TSelf(wxDb) db, TClass(wxString) tableName, TClass(wxString) privileges, TClass(wxString) userName, TClass(wxString) schema, TClass(wxString) path );
int wxDb_TranslateSqlState(TSelf(wxDb) db, TClass(wxString) sqlState);
TClass(wxDb) wxDb_Create( TClass(wxHENV) henv, TBool fwdOnlyCursors );
void wxDb_Delete( TSelf(wxDb) db );
/** Return dynamic column information about a result set of a query. */
TClass(wxDbColInfArray) wxDb_GetResultColumns( TSelf(wxDb) db, int* pnumCols );

/*-----------------------------------------------------------------------------
  DbInf
-----------------------------------------------------------------------------*/
TClassDef(wxDbInf); 
TClass(wxString) wxDbInf_GetCatalogName( TSelf(wxDbInf) self );
TClass(wxString) wxDbInf_GetSchemaName( TSelf(wxDbInf) self );
int wxDbInf_GetNumTables( TSelf(wxDbInf) self );
TClass(wxDbTableInf) wxDbInf_GetTableInf( TSelf(wxDbInf) self, int index );
void wxDbInf_Delete( TSelf(wxDbInf) self );

/*-----------------------------------------------------------------------------
  DbTableInf
-----------------------------------------------------------------------------*/
TClassDef(wxDbTableInf);
TClass(wxString) wxDbTableInf_GetTableName( TSelf(wxDbTableInf) self );
TClass(wxString) wxDbTableInf_GetTableType( TSelf(wxDbTableInf) self );
TClass(wxString) wxDbTableInf_GetTableRemarks( TSelf(wxDbTableInf) self );
int wxDbTableInf_GetNumCols( TSelf(wxDbTableInf) self );

/*-----------------------------------------------------------------------------
  DbColInfArray
-----------------------------------------------------------------------------*/
TClassDef(wxDbColInfArray);
TClass(wxDbColInf) wxDbColInfArray_GetColInf( TSelf(wxDbColInfArray) self, int index );
void wxDbColInfArray_Delete( TSelf(wxDbColInfArray) self );

/*-----------------------------------------------------------------------------
  DbColInf
-----------------------------------------------------------------------------*/
TClassDef(wxDbColInf); 
TClass(wxString) wxDbColInf_GetCatalog( TSelf(wxDbColInf) self );
TClass(wxString) wxDbColInf_GetSchema( TSelf(wxDbColInf) self );
TClass(wxString) wxDbColInf_GetTableName( TSelf(wxDbColInf) self );
TClass(wxString) wxDbColInf_GetColName( TSelf(wxDbColInf) self );
TClass(wxString) wxDbColInf_GetTypeName( TSelf(wxDbColInf) self );
TClass(wxString) wxDbColInf_GetRemarks( TSelf(wxDbColInf) self );
TClass(wxString) wxDbColInf_GetPkTableName( TSelf(wxDbColInf) self );
TClass(wxString) wxDbColInf_GetFkTableName( TSelf(wxDbColInf) self );
int wxDbColInf_GetSqlDataType( TSelf(wxDbColInf) self );
int wxDbColInf_GetColumnSize( TSelf(wxDbColInf) self );
int wxDbColInf_GetBufferLength( TSelf(wxDbColInf) self );
int wxDbColInf_GetDecimalDigits( TSelf(wxDbColInf) self );
int wxDbColInf_GetNumPrecRadix( TSelf(wxDbColInf) self );
int wxDbColInf_GetDbDataType( TSelf(wxDbColInf) self );
int wxDbColInf_GetPkCol( TSelf(wxDbColInf) self );
int wxDbColInf_GetFkCol( TSelf(wxDbColInf) self );
TBool wxDbColInf_IsNullable( TSelf(wxDbColInf) self );

