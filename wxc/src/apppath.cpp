#include "wrapper.h"
#ifdef __WXMSW__
# include <windows.h>
#elif defined(__WXMAC__)
# ifdef __DARWIN__
#  include <mach-o/dyld.h>
   typedef int (*NSGetExecutablePathProcPtr)(char *buf, size_t *bufsize);
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
    char buf[512] = "";
    GetModuleFileName(NULL, buf, 511);
    path = buf;

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
      if (strlen(buf) > 0) {
        path = buf;
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
  path = wxGetenv("APPDIR");
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
EWXWEXPORT(int, wxGetApplicationDir)(char* buffer)
{
  wxString result = GetApplicationDir();
  if (buffer) memcpy(buffer, result.c_str(), result.Length());
  return result.Length(); 
}

EWXWEXPORT(int, wxGetApplicationPath)(char* buffer)
{
  wxString result = GetApplicationPath();
  if (buffer) memcpy(buffer, result.c_str(), result.Length());
  return result.Length(); 
}
}