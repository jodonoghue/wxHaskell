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


int   wxDb_GetStatus(TSelf(wxDb) db);
TClass(wxString) wxDb_GetErrorMsg(TSelf(wxDb) db);
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

/*-----------------------------------------------------------------------------
  Global
-----------------------------------------------------------------------------*/
void  wxDb_CloseConnections();
int   wxDb_ConnectionsInUse();
TClass(wxDb)  wxDb_GetConnection( TClass(wxDbConnectInf) connectInf, TBool fwdCursorsOnly );
TBool wxDb_FreeConnection( TClass(wxDb) db);
TBool wxDb_GetDataSource( TClass(HENV) henv, void* dsn, int dsnLen, void* description, int descLen, int direction );
