#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxRadioBox_Create) (void* _prt, int _id, wxChar* _txt, int _lft, int _top, int _wdt, int _hgt, int _n, void* _str, int _dim, int _stl)
{
	wxString items[256];

	for (int i = 0; i < _n; i++)
		items[i] = ((wxChar**)_str)[i];

	return (void*) new wxRadioBox ((wxWindow*)_prt, _id, _txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _n, items, _dim, _stl, wxDefaultValidator);
}

EWXWEXPORT(int, wxRadioBox_FindString)(void* _obj, void* s)
{
	return ((wxRadioBox*)_obj)->FindString((wxChar*) s);
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
	((wxRadioBoxBase*)_obj)->SetString(item, (wxChar*)label);
#else
	((wxRadioBox*)_obj)->SetLabel(item, (wxChar*)label);
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
	wxString result = ((wxRadioBox*)_obj)->GetString(item);
#else
	wxString result = ((wxRadioBox*)_obj)->GetLabel(item);
#endif
	return copyStrToBuf(_buf, result);
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
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(void, wxRadioBox_SetStringSelection)(void* _obj, void* s)
{
	((wxRadioBox*)_obj)->SetStringSelection((wxChar*) s);
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
#if wxVERSION_NUMBER >= 2600
	return ((wxRadioBox*)_obj)->GetCount();
#else
	return ((wxRadioBox*)_obj)->GetNumberOfRowsOrCols();
#endif
}
	
EWXWEXPORT(void, wxRadioBox_SetNumberOfRowsOrCols)(void* _obj, int n)
{
#if wxVERSION_NUMBER >= 2600
	return;
#else
	((wxRadioBox*)_obj)->SetNumberOfRowsOrCols(n);
#endif
}

}
