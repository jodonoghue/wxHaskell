#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxDirDialog_Create) (void* _prt, void* _msg, void* _dir, int _lft, int _top, int _stl)
{
	return (void*) new wxDirDialog ((wxWindow*)_prt, (wxChar*)_msg, (wxChar*) _dir, _stl, wxPoint(_lft, _top));
}

EWXWEXPORT(void, wxDirDialog_SetMessage)(void* _obj, void* msg)
{
	((wxDirDialog*)_obj)->SetMessage((wxChar*)msg);
}
	
EWXWEXPORT(void, wxDirDialog_SetPath)(void* _obj, void* pth)
{
	((wxDirDialog*)_obj)->SetPath((wxChar*) pth);
}
	
EWXWEXPORT(void, wxDirDialog_SetStyle)(void* _obj, int style)
{
	((wxDirDialog*)_obj)->SetStyle((long)style);
}
	
EWXWEXPORT(int, wxDirDialog_GetMessage)(void* _obj, void* _buf)
{
	wxString result =((wxDirDialog*)_obj)->GetMessage();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxDirDialog_GetPath)(void* _obj, void* _buf)
{
	wxString result =((wxDirDialog*)_obj)->GetPath();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxDirDialog_GetStyle)(void* _obj)
{
	return (int)((wxDirDialog*)_obj)->GetStyle();
}
	
}
