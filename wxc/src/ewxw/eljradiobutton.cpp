#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxRadioButton_Create) (void* _prt, int _id, char* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxRadioButton ((wxWindow*)_prt, _id, _txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(void, wxRadioButton_SetValue)(void* _obj, int value)
{
	((wxRadioButton*)_obj)->SetValue(value != 0);
}
	
EWXWEXPORT(int, wxRadioButton_GetValue)(void* _obj)
{
	return (int)((wxRadioButton*)_obj)->GetValue();
}

} 
