#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxCheckBox_Create)(wxWindow* _prt,int _id,wxString* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxCheckBox (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(void,wxCheckBox_SetValue)(wxCheckBox* _obj,int value)
{
	_obj->SetValue(value != 0);
}
	
EWXWEXPORT(int,wxCheckBox_GetValue)(wxCheckBox* _obj)
{
	return (int)_obj->GetValue();
}

} 
