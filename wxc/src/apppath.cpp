#include "wrapper.h"
#ifdef __WXMSW__
# include <windows.h>
#elif defined(__WXMAC__)
# ifdef __DARWIN__
#  include <ApplicationServices/ApplicationServices.h>
# else
#  include <Types.h>
#  include <Files.h>
#  include <Processes.h> 
# endif
#endif

#ifdef __WXMAC__
void fss2path(char *path, FSSpec *fss)
{
  int l;             //fss->name contains name of last item in path
  for(l=0; l<(fss->name[0]); l++) path[l] = fss->name[l + 1]; 
  path[l] = 0;

  if(fss->parID != fsRtParID) //path is more than just a volume name
  { 
    int i, len;
    CInfoPBRec pb;
    
    pb.dirInfo.ioNamePtr = fss->name;
    pb.dirInfo.ioVRefNum = fss->vRefNum;
    pb.dirInfo.ioDrParID = fss->parID;
    do
    {
      pb.dirInfo.ioFDirIndex = -1;  //get parent directory name
      pb.dirInfo.ioDrDirID = pb.dirInfo.ioDrParID;   
      if(PBGetCatInfoSync(&pb) != noErr) break;

      len = fss->name[0] + 1;
      for(i=l; i>=0;  i--) path[i + len] = path[i];
      for(i=1; i<len; i++) path[i - 1] = fss->name[i]; //add to start of path
      path[i - 1] = ':';
      l += len;
    } while(pb.dirInfo.ioDrDirID != fsRtDirID); //while more directory levels
  }
}
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

/* MAC */
#elif defined(__WXMAC__)
    char buf[512] = "";
    ProcessInfoRec processinfo;
    ProcessSerialNumber procno ;
    FSSpec fsSpec;

    procno.highLongOfPSN = NULL ;
    procno.lowLongOfPSN = kCurrentProcess ;
    processinfo.processInfoLength = sizeof(ProcessInfoRec);
    processinfo.processName = NULL;
    processinfo.processAppSpec = &fsSpec;

    GetProcessInformation( &procno , &processinfo ) ;
    fss2path(buf,&fsSpec);
    path = buf;
		
/* UNIX */
#else
    wxString argv0 = wxTheApp->argv[0];

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