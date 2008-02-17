#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxCheckBox_Create) (void* _prt, int _id, wxChar* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxCheckBox ((wxWindow*)_prt, _id, _txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(void, wxCheckBox_SetValue)(void* _obj, int value)
{
	((wxCheckBox*)_obj)->SetValue(value != 0);
}
	
EWXWEXPORT(int, wxCheckBox_GetValue)(void* _obj)
{
	return (int)((wxCheckBox*)_obj)->GetValue();
}

} 
