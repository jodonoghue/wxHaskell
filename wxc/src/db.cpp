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

}