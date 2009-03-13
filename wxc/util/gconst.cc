#include "wx/dynlib.h"

void getint(wxDynamicLibrary *lib,const char* name)
{
  typedef int (*wxexpType)();
  wxString s = wxString::FromAscii(name);
  wxString s2 = _T("exp");
  wxString s3 = s2 + s;
  wxexpType pfnexp = (wxexpType)lib->GetSymbol(s3);
  if ( !pfnexp )
    {
      wxPrintf(_T("//ERROR: function '%s' wasn't found \n"),s3.c_str());
    } else {
      wxPrintf(_T("def_const_int(\"%s\",%d)\n"),s.c_str(),pfnexp());
    }
}

void getstr(wxDynamicLibrary *lib,const char* name)
{
  typedef wxString* (*wxexpstrType)();
  wxString s = wxString::FromAscii(name);
  wxString s2 = _T("exp");
  wxString s3 = s2 + s;
  wxexpstrType pfnexp = (wxexpstrType)lib->GetSymbol(s3);
  if ( !pfnexp )
    {
      wxPrintf(_T("//ERROR: function '%s' wasn't found \n"),s3.c_str());
    } else {
      wxString* sp = pfnexp();
      wxPrintf(_T("def_const_str(\"%s\",\"%s\")\n"),s.c_str(),sp->c_str());
      delete sp;
    }
}

int main(void)
{
  static const wxChar *LIB_NAME = _T("./libwxc.so");

  wxDynamicLibrary lib(LIB_NAME);
  if ( !lib.IsLoaded() )
    {
      wxPrintf(_T("ERROR: failed to load '%s'.\n"), LIB_NAME);
    }
  else
    {
#define  def_const_int(name,value) {\
  getint(&lib,name); \
}

#define  def_const_str(name,value) { \
  getstr(&lib,name); \
}

#include "const_.h"
    }
}
