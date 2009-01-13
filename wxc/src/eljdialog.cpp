#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxDialog_Create) (wxWindow* _prt, int _id, wxString* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxDialog (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(bool,wxDialog_IsModal)(wxDialog* _obj)
{
	return _obj->IsModal();
}
	
EWXWEXPORT(int,wxDialog_ShowModal)(void* _obj)
{
	return ((wxDialog*)_obj)->ShowModal();
}
	
EWXWEXPORT(void,wxDialog_EndModal)(void* _obj,int retCode)
{
	((wxDialog*)_obj)->EndModal(retCode);
}
	
EWXWEXPORT(void,wxDialog_SetReturnCode)(void* _obj,int returnCode)
{
	((wxDialog*)_obj)->SetReturnCode(returnCode);
}
	
EWXWEXPORT(int,wxDialog_GetReturnCode)(void* _obj)
{
	return ((wxDialog*)_obj)->GetReturnCode();
}
	
#if (wxVERSION_NUMBER >= 2400) && (wxVERSION_NUMBER < 2800)
EWXWEXPORT(void,wxDialog_SetIcons)(void* _obj,void* _icons)
{
	((wxDialog*)_obj)->SetIcons(*((wxIconBundle*)_icons));
}
#endif

}
