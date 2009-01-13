#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/cshelp.h"

extern "C"
{

EWXWEXPORT(void*,wxContextHelp_Create)(wxWindow* win,bool beginHelp)
{
	return (void*)new wxContextHelp(win, beginHelp);
}
	
EWXWEXPORT(void,wxContextHelp_Delete)(void* _obj)
{
	delete (wxContextHelp*)_obj;
}
	
EWXWEXPORT(int,wxContextHelp_BeginContextHelp)(void* _obj,wxWindow* win)
{
	return (int)((wxContextHelp*)_obj)->BeginContextHelp(win);
}
	
EWXWEXPORT(int,wxContextHelp_EndContextHelp)(void* _obj)
{
	return (int)((wxContextHelp*)_obj)->EndContextHelp();
}
	

EWXWEXPORT(void*,wxContextHelpButton_Create)(wxWindow* parent,int id,int x,int y,int w,int h,long style)
{
	return (void*)new wxContextHelpButton(parent, (wxWindowID)id,wxPoint(x, y), wxSize(w, h), style);
}


EWXWEXPORT(void*,wxHelpProvider_Get)()
{
	return (void*)wxHelpProvider::Get();
}
	
EWXWEXPORT(void*,wxHelpProvider_Set)(void* helpProvider)
{
	return (void*)wxHelpProvider::Set((wxHelpProvider*)helpProvider);
}
	
EWXWEXPORT(wxString*,wxHelpProvider_GetHelp)(void* _obj,void* window)
{
	wxString *result = new wxString();
	*result = ((wxHelpProvider*)_obj)->GetHelp((wxWindowBase*)window);
	return result;
}
	
EWXWEXPORT(int,wxHelpProvider_ShowHelp)(void* _obj,void* window)
{
	return (int)((wxHelpProvider*)_obj)->ShowHelp((wxWindowBase*)window);
}
	
EWXWEXPORT(void,wxHelpProvider_AddHelp)(void* _obj,void* window,wxString* text)
{
	((wxHelpProvider*)_obj)->AddHelp((wxWindowBase*)window, *text);
}
	
EWXWEXPORT(void,wxHelpProvider_AddHelpById)(void* _obj,int id,wxString* text)
{
	((wxHelpProvider*)_obj)->AddHelp((wxWindowID)id, *text);
}
	
EWXWEXPORT(void,wxHelpProvider_RemoveHelp)(void* _obj,void* window)
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
	
EWXWEXPORT(void,wxHelpControllerHelpProvider_SetHelpController)(void* _obj,void* hc)
{
	((wxHelpControllerHelpProvider*)_obj)->SetHelpController((wxHelpControllerBase*)hc);
}
	
EWXWEXPORT(void*,wxHelpControllerHelpProvider_GetHelpController)(void* _obj)
{
	return (void*)((wxHelpControllerHelpProvider*)_obj)->GetHelpController();
}
	
}
#endif
