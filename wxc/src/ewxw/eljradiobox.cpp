#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxRadioBox_Create) (void* _prt, int _id, char* _txt, int _lft, int _top, int _wdt, int _hgt, int _n, void* _str, int _dim, int _stl)
{
	wxString items[256];

	for (int i = 0; i < _n; i++)
		items[i] = ((char**)_str)[i];

	return (void*) new wxRadioBox ((wxWindow*)_prt, _id, _txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _n, items, _dim, _stl, wxDefaultValidator);
}

EWXWEXPORT(int, wxRadioBox_FindString)(void* _obj, void* s)
{
	return ((wxRadioBox*)_obj)->FindString((char*) s);
}
	
EWXWEXPORT(void, wxRadioBox_SetSelection)(void* _obj, int _n)
{
	((wxRadioBox*)_obj)->SetSelection(_n);
}
	
EWXWEXPORT(int, wxRadioBox_GetSelection)(void* _obj)
{
	return ((wxRadioBox*)_obj)->GetSelection();
}
	
EWXWEXPORT(void, wxRadioBox_SetItemLabel)(void* _obj, int item, void* label)
{
#if wxVERSION_NUMBER >= 2400
	((wxRadioBoxBase*)_obj)->SetString(item, (char*)label);
#else
	((wxRadioBox*)_obj)->SetLabel(item, (char*)label);
#endif
}
	
EWXWEXPORT(void, wxRadioBox_SetItemBitmap)(void* _obj, int item, void* bitmap)
{
#if wxVERSION_NUMBER < 2400
	((wxRadioBox*)_obj)->SetLabel(item, (wxBitmap*) bitmap);
#endif
}
	
EWXWEXPORT(int, wxRadioBox_GetItemLabel)(void* _obj, int item, void* _buf)
{
#if wxVERSION_NUMBER >= 2400
	wxString result = ((wxRadioBoxBase*)_obj)->GetString(item);
#else
	wxString result = ((wxRadioBox*)_obj)->GetLabel(item);
#endif
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(void, wxRadioBox_EnableItem)(void* _obj, int item, int enable)
{
	((wxRadioBox*)_obj)->Enable(item, enable != 0);
}
	
EWXWEXPORT(void, wxRadioBox_ShowItem)(void* _obj, int item, int show)
{
	((wxRadioBox*)_obj)->Show(item, show != 0);
}
	
EWXWEXPORT(int, wxRadioBox_GetStringSelection)(void* _obj, void* _buf)
{
	wxString result = ((wxRadioBox*)_obj)->GetStringSelection();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(void, wxRadioBox_SetStringSelection)(void* _obj, void* s)
{
	((wxRadioBox*)_obj)->SetStringSelection((char*) s);
}
	
EWXWEXPORT(int, wxRadioBox_Number)(void* _obj)
{
#if wxVERSION_NUMBER >= 2400
	return ((wxRadioBox*)_obj)->GetCount();
#else
	return ((wxRadioBox*)_obj)->Number();
#endif
}
	
EWXWEXPORT(int, wxRadioBox_GetNumberOfRowsOrCols)(void* _obj)
{
	return ((wxRadioBox*)_obj)->GetNumberOfRowsOrCols();
}
	
EWXWEXPORT(void, wxRadioBox_SetNumberOfRowsOrCols)(void* _obj, int n)
{
	((wxRadioBox*)_obj)->SetNumberOfRowsOrCols(n);
}

}
