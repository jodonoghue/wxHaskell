#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxStaticLine_Create) (void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxStaticLine ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int, wxStaticLine_IsVertical) (void* _obj)
{
	return (int)((wxStaticLine*)_obj)->IsVertical ();
}
	
EWXWEXPORT(int, wxStaticLine_GetDefaultSize) (void* _obj)
{
	return ((wxStaticLine*)_obj)->GetDefaultSize ();
}

}
