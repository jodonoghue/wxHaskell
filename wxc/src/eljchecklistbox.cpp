#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxCheckListBox_Create) (void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _n, void* _str, int _stl)
{
	wxCheckListBox* result = new wxCheckListBox ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), 0, NULL, _stl, wxDefaultValidator);

	for (int i = 0; i < _n; i++)
		result->Append(((wxChar**)_str)[i]);

	return (void*) result;
}

EWXWEXPORT(void, wxCheckListBox_Check)(void* _obj, int item, int check)
{
	((wxCheckListBox*)_obj)->Check(item, check != 0);
}
	
EWXWEXPORT(int, wxCheckListBox_IsChecked)(void* _obj, int item)
{
	return (int)((wxCheckListBox*)_obj)->IsChecked(item);
}

}
