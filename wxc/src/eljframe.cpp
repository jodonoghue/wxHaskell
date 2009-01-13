#include "wrapper.h"

extern "C"
{

EWXWEXPORT(bool,wxTopLevelWindow_EnableCloseButton)(wxTopLevelWindow* _obj,bool enable)
{
  return _obj->EnableCloseButton(enable);
}

EWXWEXPORT(void*,wxTopLevelWindow_GetDefaultButton)(void* _obj)
{
  return (void*)((wxTopLevelWindow*)_obj)->GetDefaultItem();
}

EWXWEXPORT(void*,wxTopLevelWindow_GetDefaultItem)(void* _obj)
{
  return (void*)((wxTopLevelWindow*)_obj)->GetDefaultItem();
}

EWXWEXPORT(void*,wxTopLevelWindow_GetIcon)(void* _obj)
{
  static wxIcon tmp = ((wxTopLevelWindow*)_obj)->GetIcon();
  return (void*) &tmp;
}

EWXWEXPORT(wxString*,wxTopLevelWindow_GetTitle)(void* _obj)
{
  wxString *result = new wxString();
  *result = ((wxTopLevelWindow*)_obj)->GetTitle();
  return result;
}

EWXWEXPORT(void,wxTopLevelWindow_Iconize)(void* _obj,bool iconize)
{
  ((wxTopLevelWindow*)_obj)->Iconize(iconize);
}

EWXWEXPORT(bool,wxTopLevelWindow_IsActive)(wxTopLevelWindow* _obj)
{
  return _obj->IsActive();
}

EWXWEXPORT(bool,wxTopLevelWindow_IsIconized)(wxTopLevelWindow* _obj)
{
  return _obj->IsIconized();
}

EWXWEXPORT(bool,wxTopLevelWindow_IsMaximized)(wxTopLevelWindow* _obj)
{
  return _obj->IsMaximized();
}

EWXWEXPORT(void,wxTopLevelWindow_Maximize)(void* _obj,bool iconize)
{
  ((wxTopLevelWindow*)_obj)->Maximize(iconize);
}

EWXWEXPORT(void,wxTopLevelWindow_RequestUserAttention)(void* _obj,int flags)
{
  ((wxTopLevelWindow*)_obj)->RequestUserAttention(flags);
}

EWXWEXPORT(void,wxTopLevelWindow_SetDefaultButton)(void* _obj,void* _item)
{
  ((wxTopLevelWindow*)_obj)->SetDefaultItem((wxButton*) _item);
}

EWXWEXPORT(void,wxTopLevelWindow_SetDefaultItem)(void* _obj,wxWindow* _item)
{
  ((wxTopLevelWindow*)_obj)->SetDefaultItem( _item);
}

EWXWEXPORT(void,wxTopLevelWindow_SetIcon)(void* _obj,void* _icons)
{
  ((wxTopLevelWindow*)_obj)->SetIcon(*((wxIcon*)_icons));
}

EWXWEXPORT(void,wxTopLevelWindow_SetIcons)(void* _obj,void* _icons)
{
  ((wxTopLevelWindow*)_obj)->SetIcons(*((wxIconBundle*)_icons));
}

EWXWEXPORT(void,wxTopLevelWindow_SetMaxSize)(void* _obj,int _w,int _h)
{
  ((wxTopLevelWindow*)_obj)->SetMaxSize(wxSize(_w, _h));
}
           
EWXWEXPORT(void,wxTopLevelWindow_SetMinSize)(void* _obj,int _w,int _h)
{
  ((wxTopLevelWindow*)_obj)->SetMinSize(wxSize(_w, _h));
}

EWXWEXPORT(void,wxTopLevelWindow_SetTitle)(void* _obj,wxString* _str)
{
  ((wxTopLevelWindow*)_obj)->SetTitle(*_str);
}

EWXWEXPORT(void*,wxFrame_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxFrame ((wxWindow*)_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void*,wxFrame_CreateStatusBar)(void* _obj,int number,int style)
{
	return (void*)((wxFrame*)_obj)->CreateStatusBar(number, style, 1);
}
	
#if wxVERSION_NUMBER >= 2800
EWXWEXPORT(void,wxFrame_Maximize)(void* _obj)
{
	((wxFrame*)_obj)->Maximize();
}
#endif
	
EWXWEXPORT(void,wxFrame_Restore)(void* _obj)
{
	((wxFrame*)_obj)->Restore();
}
	
#if wxVERSION_NUMBER >= 2800
EWXWEXPORT(void,wxFrame_Iconize)(void* _obj)
{
	((wxFrame*)_obj)->Iconize();
}
	
EWXWEXPORT(bool,wxFrame_IsMaximized)(wxFrame* _obj)
{
	return _obj->IsMaximized();
}
	
EWXWEXPORT(bool,wxFrame_IsIconized)(wxFrame* _obj)
{
	return _obj->IsIconized();
}
	
EWXWEXPORT(void*,wxFrame_GetIcon)(void* _obj)
{
	return (void*) (&((wxFrame*)_obj)->GetIcon());
}
	
EWXWEXPORT(void,wxFrame_SetIcon)(void* _obj,void* _icon)
{
	((wxFrame*)_obj)->SetIcon(*((wxIcon*) _icon));
}
#endif
	
EWXWEXPORT(int,wxFrame_GetClientAreaOrigin_left)(void* _obj)
{
	return ((wxFrame*)_obj)->GetClientAreaOrigin().x;
}
	
EWXWEXPORT(int,wxFrame_GetClientAreaOrigin_top)(void* _obj)
{
	return ((wxFrame*)_obj)->GetClientAreaOrigin().y;
}
	
EWXWEXPORT(void,wxFrame_SetMenuBar)(void* _obj,void* menubar)
{
	((wxFrame*)_obj)->SetMenuBar((wxMenuBar*)menubar);
}
	
EWXWEXPORT(void*,wxFrame_GetMenuBar)(void* _obj)
{
	return (void*)((wxFrame*)_obj)->GetMenuBar();
}
	
EWXWEXPORT(void*,wxFrame_GetStatusBar)(void* _obj)
{
	return (void*)((wxFrame*)_obj)->GetStatusBar();
}
	
EWXWEXPORT(void,wxFrame_SetStatusBar)(void* _obj,void* statBar)
{
	((wxFrame*)_obj)->SetStatusBar((wxStatusBar*) statBar);
}
	
EWXWEXPORT(void,wxFrame_SetStatusText)(void* _obj,wxString* _txt,int _number)
{
	((wxFrame*)_obj)->SetStatusText(*_txt, _number);
}
	
EWXWEXPORT(void,wxFrame_SetStatusWidths)(void* _obj,int _n,void* _widths_field)
{
	((wxFrame*)_obj)->SetStatusWidths(_n, (int*) _widths_field);
}
	
EWXWEXPORT(void*,wxFrame_CreateToolBar)(void* _obj,long style)
{
	return (void*)((wxFrame*)_obj)->CreateToolBar(style, 1);
}
	
EWXWEXPORT(void*,wxFrame_GetToolBar)(void* _obj)
{
	return (void*)((wxFrame*)_obj)->GetToolBar();
}
	
EWXWEXPORT(void,wxFrame_SetToolBar)(void* _obj,void* _toolbar)
{
	((wxFrame*)_obj)->SetToolBar((wxToolBar*) _toolbar);
}

#if wxVERSION_NUMBER >= 2400 && wxVERSION_NUMBER < 2800
EWXWEXPORT(void,wxFrame_SetIcons)(void* _obj,void* _icons)
{
	((wxFrame*)_obj)->SetIcons(*((wxIconBundle*)_icons));
}
#endif

}
