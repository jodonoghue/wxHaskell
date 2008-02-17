#include "wrapper.h"
#include "wx/process.h"

extern "C"

{

EWXWEXPORT(void*, wxProcess_CreateDefault)(void* _prt, int _id)
{
	return (void*)new wxProcess ((wxEvtHandler*)_prt, _id);
}

EWXWEXPORT(void*, wxProcess_CreateRedirect)(void* _prt, int _rdr)
{
	return (void*)new wxProcess ((wxEvtHandler*)_prt, _rdr != 0);
}

EWXWEXPORT(void, wxProcess_Delete)(void* _obj)
{
    delete (wxProcess*)_obj;
}

EWXWEXPORT(void, wxProcess_Redirect)(void* _obj)
{
	((wxProcess*)_obj)->Redirect();
}
	
EWXWEXPORT(int, wxProcess_IsRedirected)(void* _obj)
{
	return (int)((wxProcess*)_obj)->IsRedirected();
}
	
EWXWEXPORT(void, wxProcess_Detach)(void* _obj)
{
	((wxProcess*)_obj)->Detach();
}
	
EWXWEXPORT(void*, wxProcess_GetInputStream)(void* _obj)
{
	return (void*)((wxProcess*)_obj)->GetInputStream();
}
	
EWXWEXPORT(void*, wxProcess_GetErrorStream)(void* _obj)
{
	return (void*)((wxProcess*)_obj)->GetErrorStream();
}
	
EWXWEXPORT(void*, wxProcess_GetOutputStream)(void* _obj)
{
	return (void*)((wxProcess*)_obj)->GetOutputStream();
}
	
EWXWEXPORT(void, wxProcess_CloseOutput)(void* _obj)
{
	((wxProcess*)_obj)->CloseOutput();
}
	

EWXWEXPORT(int, wxProcessEvent_GetPid)(void* _obj)
{
	return ((wxProcessEvent*)_obj)->GetPid();
}

EWXWEXPORT(int, wxProcessEvent_GetExitCode)(void* _obj)
{
	return ((wxProcessEvent*)_obj)->GetExitCode();
}


EWXWEXPORT(int, wxStreamBase_GetLastError)(void* _obj)
{
	return (int)((wxStreamBase*)_obj)->GetLastError();
}
	
EWXWEXPORT(int, wxStreamBase_IsOk)(void* _obj)
{
	return (int)((wxStreamBase*)_obj)->IsOk();
}
	
EWXWEXPORT(int, wxStreamBase_GetSize)(void* _obj)
{
	return (int)((wxStreamBase*)_obj)->GetSize();
}
	

EWXWEXPORT(void, wxOutputStream_Delete)(void* _obj)
{
	delete (wxOutputStream*)_obj;
}

EWXWEXPORT(void, wxOutputStream_PutC)(void* _obj, char c)
{
	((wxOutputStream*)_obj)->PutC(c);
}
	
EWXWEXPORT(void, wxOutputStream_Write)(void* _obj, void* buffer, int size)
{
	((wxOutputStream*)_obj)->Write((const void*)buffer, (size_t)size);
}
	
EWXWEXPORT(int, wxOutputStream_Seek)(void* _obj, int pos, int mode)
{
	return (int)((wxOutputStream*)_obj)->SeekO((off_t)pos, (wxSeekMode)mode);
}
	
EWXWEXPORT(int, wxOutputStream_Tell)(void* _obj)
{
	return (int)((wxOutputStream*)_obj)->TellO();
}
	
EWXWEXPORT(int, wxOutputStream_LastWrite)(void* _obj)
{
	return (int)((wxOutputStream*)_obj)->LastWrite();
}
	
EWXWEXPORT(void, wxOutputStream_Sync)(void* _obj)
{
	((wxOutputStream*)_obj)->Sync();
}
	

EWXWEXPORT(void, wxInputStream_Delete)(void* _obj)
{
	delete (wxInputStream*)_obj;
}

EWXWEXPORT(int, wxInputStream_Eof)(void* _obj)
{
	return (int)((wxInputStream*)_obj)->Eof();
}
	
EWXWEXPORT(char, wxInputStream_Peek)(void* _obj)
{
	return ((wxInputStream*)_obj)->Peek();
}
	
EWXWEXPORT(char, wxInputStream_GetC)(void* _obj)
{
	return ((wxInputStream*)_obj)->GetC();
}
	
EWXWEXPORT(void, wxInputStream_Read)(void* _obj, void *buffer, int size)
{
	((wxInputStream*)_obj)->Read(buffer, (size_t)size);
}
	
EWXWEXPORT(int, wxInputStream_SeekI)(void* _obj, int pos, int mode)
{
	return (int)((wxInputStream*)_obj)->SeekI((off_t)pos, (wxSeekMode)mode);
}
	
EWXWEXPORT(int, wxInputStream_Tell)(void* _obj)
{
	return (int)((wxInputStream*)_obj)->TellI();
}
	
EWXWEXPORT(int, wxInputStream_LastRead)(void* _obj)
{
	return (int)((wxInputStream*)_obj)->LastRead();
}	

EWXWEXPORT(int, wxInputStream_UngetBuffer)(void* _obj, void* buffer, int size)
{
	return (int)((wxInputStream*)_obj)->Ungetch((const void*)buffer, (size_t)size);
}
	
EWXWEXPORT(int, wxInputStream_Ungetch)(void* _obj, char c)
{
	return (int)((wxInputStream*)_obj)->Ungetch(c);
}
	
}
