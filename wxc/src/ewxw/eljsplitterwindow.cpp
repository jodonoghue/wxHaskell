#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxSplitterWindow_Create) (void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxSplitterWindow ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void*, wxSplitterWindow_GetWindow1)(void* _obj)
{
	return (void*)((wxSplitterWindow*)_obj)->GetWindow1();
}
	
EWXWEXPORT(void*, wxSplitterWindow_GetWindow2)(void* _obj)
{
	return (void*)((wxSplitterWindow*)_obj)->GetWindow2();
}
	
EWXWEXPORT(void, wxSplitterWindow_SetSplitMode)(void* _obj, int mode)
{
	((wxSplitterWindow*)_obj)->SetSplitMode(mode);
}
	
EWXWEXPORT(int, wxSplitterWindow_GetSplitMode)(void* _obj)
{
	return ((wxSplitterWindow*)_obj)->GetSplitMode();
}
	
EWXWEXPORT(void, wxSplitterWindow_Initialize)(void* _obj, void* window)
{
	((wxSplitterWindow*)_obj)->Initialize((wxWindow*)window);
}
	
EWXWEXPORT(int, wxSplitterWindow_SplitVertically)(void* _obj, void* window1, void* window2, int sashPosition)
{
	return (int)((wxSplitterWindow*)_obj)->SplitVertically((wxWindow*)window1, (wxWindow*)window2, sashPosition);
}
	
EWXWEXPORT(int, wxSplitterWindow_SplitHorizontally)(void* _obj, void* window1, void* window2, int sashPosition)
{
	return (int)((wxSplitterWindow*)_obj)->SplitHorizontally((wxWindow*)window1, (wxWindow*)window2, sashPosition);
}
	
EWXWEXPORT(int, wxSplitterWindow_Unsplit)(void* _obj, void* toRemove)
{
	return (int)((wxSplitterWindow*)_obj)->Unsplit((wxWindow*)toRemove);
}
	
EWXWEXPORT(int, wxSplitterWindow_ReplaceWindow)(void* _obj, void* winOld, void* winNew)
{
	return (int)((wxSplitterWindow*)_obj)->ReplaceWindow((wxWindow*)winOld, (wxWindow*)winNew);
}
	
EWXWEXPORT(int, wxSplitterWindow_IsSplit)(void* _obj)
{
	return (int)((wxSplitterWindow*)_obj)->IsSplit();
}
	
EWXWEXPORT(void, wxSplitterWindow_SetSashSize)(void* _obj, int width)
{
	((wxSplitterWindow*)_obj)->SetSashSize(width);
}
	
EWXWEXPORT(void, wxSplitterWindow_SetBorderSize)(void* _obj, int width)
{
	((wxSplitterWindow*)_obj)->SetBorderSize(width);
}
	
EWXWEXPORT(int, wxSplitterWindow_GetSashSize)(void* _obj)
{
	return ((wxSplitterWindow*)_obj)->GetSashSize();
}
	
EWXWEXPORT(int, wxSplitterWindow_GetBorderSize)(void* _obj)
{
	return ((wxSplitterWindow*)_obj)->GetBorderSize();
}
	
EWXWEXPORT(void, wxSplitterWindow_SetSashPosition)(void* _obj, int position, int redraw)
{
	((wxSplitterWindow*)_obj)->SetSashPosition(position, redraw != 0);
}
	
EWXWEXPORT(int, wxSplitterWindow_GetSashPosition)(void* _obj)
{
	return ((wxSplitterWindow*)_obj)->GetSashPosition();
}
	
EWXWEXPORT(void, wxSplitterWindow_SetMinimumPaneSize)(void* _obj, int min)
{
	((wxSplitterWindow*)_obj)->SetMinimumPaneSize(min);
}
	
EWXWEXPORT(int, wxSplitterWindow_GetMinimumPaneSize)(void* _obj)
{
	return ((wxSplitterWindow*)_obj)->GetMinimumPaneSize();
}
	
}
