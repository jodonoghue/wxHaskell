#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxFrame_Create) (void* _prt, int _id, char* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxFrame ((wxWindow*)_prt, _id, _txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void*, wxFrame_CreateStatusBar)(void* _obj, int number, int style)
{
	return (void*)((wxFrame*)_obj)->CreateStatusBar(number, style, 1);
}
	
EWXWEXPORT(void, wxFrame_Maximize)(void* _obj)
{
	((wxFrame*)_obj)->Maximize();
}
	
EWXWEXPORT(void, wxFrame_Restore)(void* _obj)
{
	((wxFrame*)_obj)->Restore();
}
	
EWXWEXPORT(void, wxFrame_Iconize)(void* _obj)
{
	((wxFrame*)_obj)->Iconize();
}
	
EWXWEXPORT(int, wxFrame_IsMaximized)(void* _obj)
{
	return (int)((wxFrame*)_obj)->IsMaximized();
}
	
EWXWEXPORT(int, wxFrame_IsIconized)(void* _obj)
{
	return (int)((wxFrame*)_obj)->IsIconized();
}
	
EWXWEXPORT(void*, wxFrame_GetIcon)(void* _obj)
{
	return (void*) (&((wxFrame*)_obj)->GetIcon());
}
	
EWXWEXPORT(void, wxFrame_SetIcon)(void* _obj, void* _icon)
{
	((wxFrame*)_obj)->SetIcon(*((wxIcon*) _icon));
}
	
EWXWEXPORT(int, wxFrame_GetClientAreaOrigin_left)(void* _obj)
{
	return ((wxFrame*)_obj)->GetClientAreaOrigin().x;
}
	
EWXWEXPORT(int, wxFrame_GetClientAreaOrigin_top)(void* _obj)
{
	return ((wxFrame*)_obj)->GetClientAreaOrigin().y;
}
	
EWXWEXPORT(void, wxFrame_SetMenuBar)(void* _obj, void* menubar)
{
	((wxFrame*)_obj)->SetMenuBar((wxMenuBar*)menubar);
}
	
EWXWEXPORT(void*, wxFrame_GetMenuBar)(void* _obj)
{
	return (void*)((wxFrame*)_obj)->GetMenuBar();
}
	
EWXWEXPORT(void*, wxFrame_GetStatusBar)(void* _obj)
{
	return (void*)((wxFrame*)_obj)->GetStatusBar();
}
	
EWXWEXPORT(void, wxFrame_SetStatusBar)(void* _obj, void* statBar)
{
	((wxFrame*)_obj)->SetStatusBar((wxStatusBar*) statBar);
}
	
EWXWEXPORT(void, wxFrame_SetStatusText)(void* _obj, char* _txt, int _number)
{
	((wxFrame*)_obj)->SetStatusText(_txt, _number);
}
	
EWXWEXPORT(void, wxFrame_SetStatusWidths)(void* _obj, int _n, void* _widths_field)
{
	((wxFrame*)_obj)->SetStatusWidths(_n, (int*) _widths_field);
}
	
EWXWEXPORT(void*, wxFrame_CreateToolBar)(void* _obj, long style)
{
	return (void*)((wxFrame*)_obj)->CreateToolBar(style, 1);
}
	
EWXWEXPORT(void*, wxFrame_GetToolBar)(void* _obj)
{
	return (void*)((wxFrame*)_obj)->GetToolBar();
}
	
EWXWEXPORT(void, wxFrame_SetToolBar)(void* _obj, void* _toolbar)
{
	((wxFrame*)_obj)->SetToolBar((wxToolBar*) _toolbar);
}

#if wxVERSION_NUMBER >= 2400
EWXWEXPORT(void, wxFrame_SetIcons)(void* _obj, void* _icons)
{
	((wxFrame*)_obj)->SetIcons(*((wxIconBundle*)_icons));
}
#endif

}
