#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxDialog_Create) (void* _prt, int _id, char* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxDialog ((wxWindow*)_prt, _id, _txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int, wxDialog_IsModal)(void* _obj)
{
	return (int)((wxDialog*)_obj)->IsModal();
}
	
EWXWEXPORT(int, wxDialog_ShowModal)(void* _obj)
{
	return ((wxDialog*)_obj)->ShowModal();
}
	
EWXWEXPORT(void, wxDialog_EndModal)(void* _obj, int retCode)
{
	((wxDialog*)_obj)->EndModal(retCode);
}
	
EWXWEXPORT(void, wxDialog_SetReturnCode)(void* _obj, int returnCode)
{
	((wxDialog*)_obj)->SetReturnCode(returnCode);
}
	
EWXWEXPORT(int, wxDialog_GetReturnCode)(void* _obj)
{
	return ((wxDialog*)_obj)->GetReturnCode();
}
	
#if wxVERSION_NUMBER >= 2400
EWXWEXPORT(void, wxDialog_SetIcons)(void* _obj, void* _icons)
{
	((wxDialog*)_obj)->SetIcons(*((wxIconBundle*)_icons));
}
#endif

}
