#include "wrapper.h"
#ifdef __WXMSW__
# include <windows.h>
#elif defined(__WXMAC__)
# ifdef __DARWIN__
#  include <mach-o/dyld.h>
   typedef int (*NSGetExecutablePathProcPtr)(char* buf, size_t* bufsize);
# else
#  include <Types.h>
#  include <Files.h>
#  include <Processes.h> 
# endif
#endif


wxString GetApplicationPath()
{
  static bool found = false;
  static wxString path;

  if (!found)
  {
/* Windows */
#ifdef __WXMSW__
    wxChar buf[512] = wxT("");
    GetModuleFileName(NULL, buf, 511);
    path = wxString(buf, wxConvUTF8);

/* UNIX & MAC*/
#else
    wxString argv0;

# ifdef __WXMAC__		
    if (NSIsSymbolNameDefined("__NSGetExecutablePath"))
    {
      char buf[512];
      size_t bufLen = 512;
      buf[0] = 0;
      ((NSGetExecutablePathProcPtr) NSAddressOfSymbol(NSLookupAndBindSymbol("__NSGetExecutablePath")))(buf, &bufLen);
      wxString strBuf = wxString();
      size_t actualBuflen = strlen(buf);
      if (actualBuflen > 0) {
        // FIXME: we *assume* that the NS stuff returns utf-8 encoded strings
        path = wxString(buf, wxConvUTF8);
        found=true;
        return path;
      }
    }
# endif

    argv0 = wxTheApp->argv[0];

    /* check absolute path */
    if (wxIsAbsolutePath(argv0)) {
        path = argv0;
    }
    else {
      /* check relative path */
      wxString fname = wxGetCwd() + wxFILE_SEP_PATH + argv0;
      if (wxFileExists(fname)) {
        path = fname;
      } else {
        /* find on PATH */
        wxPathList pathlist;
        pathlist.AddEnvList(wxT("PATH"));
        path = pathlist.FindAbsoluteValidPath(argv0);
      }
    }

    wxFileName filename(path);
    filename.Normalize();
    path = filename.GetFullPath();
#endif

    found = true;
  }
  return path;
}


wxString GetApplicationDir()
{
  wxString path;
  
  /* check APPDIR on unix's */
#ifndef __WXMSW__
# ifndef __WXMAC__
  wxGetEnv(wxT("APPDIR"), &path);
  if (!path.IsEmpty()) return path;
# endif
#endif

  path = GetApplicationPath();
  if (path.IsEmpty())
    return wxGetCwd();
  else {
    wxFileName fname(path);
    return fname.GetPath(wxPATH_GET_VOLUME);
  }  
}

extern "C" 
{
EWXWEXPORT(wxString*,wxGetApplicationDir)()
{
  wxString *result = new wxString();
  *result = GetApplicationDir();
  return result;
}

EWXWEXPORT(wxString*,wxGetApplicationPath)()
{
  wxString *result = new wxString();
  *result = GetApplicationPath();
  return result;
}
}
