#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/cshelp.h"

extern "C"
{

EWXWEXPORT(void*,wxContextHelp_Create)(void* win, int beginHelp)
{
	return (void*)new wxContextHelp((wxWindow*)win, beginHelp != 0);
}
	
EWXWEXPORT(void,wxContextHelp_Delete)(void* _obj)
{
	delete (wxContextHelp*)_obj;
}
	
EWXWEXPORT(int,wxContextHelp_BeginContextHelp)(void* _obj, void* win)
{
	return (int)((wxContextHelp*)_obj)->BeginContextHelp((wxWindow*)win);
}
	
EWXWEXPORT(int,wxContextHelp_EndContextHelp)(void* _obj)
{
	return (int)((wxContextHelp*)_obj)->EndContextHelp();
}
	

EWXWEXPORT(void*,wxContextHelpButton_Create)(void* parent, int id, int x, int y, int w, int h, long style)
{
	return (void*)new wxContextHelpButton((wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), style);
}


EWXWEXPORT(void*,wxHelpProvider_Get)()
{
	return (void*)wxHelpProvider::Get();
}
	
EWXWEXPORT(void*,wxHelpProvider_Set)(void* helpProvider)
{
	return (void*)wxHelpProvider::Set((wxHelpProvider*)helpProvider);
}
	
EWXWEXPORT(int,wxHelpProvider_GetHelp)(void* _obj, void* window, void* _ref)
{
	wxString tmp = ((wxHelpProvider*)_obj)->GetHelp((wxWindowBase*)window);
	if (_ref)
		memcpy (_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int,wxHelpProvider_ShowHelp)(void* _obj, void* window)
{
	return (int)((wxHelpProvider*)_obj)->ShowHelp((wxWindowBase*)window);
}
	
EWXWEXPORT(void,wxHelpProvider_AddHelp)(void* _obj, void* window, void* text)
{
	((wxHelpProvider*)_obj)->AddHelp((wxWindowBase*)window, (char*)text);
}
	
EWXWEXPORT(void,wxHelpProvider_AddHelpById)(void* _obj, int id, void* text)
{
	((wxHelpProvider*)_obj)->AddHelp((wxWindowID)id, (char*)text);
}
	
EWXWEXPORT(void,wxHelpProvider_RemoveHelp)(void* _obj, void* window)
{
	((wxHelpProvider*)_obj)->RemoveHelp((wxWindowBase*)window);
}
	
EWXWEXPORT(void,wxHelpProvider_Delete)(void* _obj)
{
	delete (wxHelpProvider*)_obj;
}


EWXWEXPORT(void*,wxSimpleHelpProvider_Create)()
{
	return (void*)new wxSimpleHelpProvider();
}
	

EWXWEXPORT(void*,wxHelpControllerHelpProvider_Create)(void* ctr)
{
	return (void*)new wxHelpControllerHelpProvider((wxHelpControllerBase*)ctr);
}
	
EWXWEXPORT(void,wxHelpControllerHelpProvider_SetHelpController)(void* _obj, void* hc)
{
	((wxHelpControllerHelpProvider*)_obj)->SetHelpController((wxHelpControllerBase*)hc);
}
	
EWXWEXPORT(void*,wxHelpControllerHelpProvider_GetHelpController)(void* _obj)
{
	return (void*)((wxHelpControllerHelpProvider*)_obj)->GetHelpController();
}
	
}
#endif
