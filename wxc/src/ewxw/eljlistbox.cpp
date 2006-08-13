#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxListBox_Create) (void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _n, void* _str, int _stl)
{
	wxListBox* result = new wxListBox ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), 0, NULL, _stl, wxDefaultValidator);

	for (int i = 0; i < _n; i++)
		result->Append(((wxChar**)_str)[i]);

	return (void*) result;
}

EWXWEXPORT(void, wxListBox_Clear)(void* _obj)
{
	((wxListBox*)_obj)->Clear();
}
	
EWXWEXPORT(void, wxListBox_Delete)(void* _obj, int n)
{
	((wxListBox*)_obj)->Delete(n);
}
	
EWXWEXPORT(int, wxListBox_GetCount)(void* _obj)
{
	return ((wxListBox*)_obj)->GetCount();
}
	
EWXWEXPORT(int, wxListBox_GetString)(void* _obj, int n, void* _buf)
{
	wxString result = ((wxListBox*)_obj)->GetString(n);
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(void, wxListBox_SetString)(void* _obj, int n, wxChar* s)
{
	((wxListBox*)_obj)->SetString(n, s);
}
	
EWXWEXPORT(int, wxListBox_FindString)(void* _obj, wxChar* s)
{
	return ((wxListBox*)_obj)->FindString(s);
}
	
EWXWEXPORT(int, wxListBox_IsSelected)(void* _obj, int n)
{
	return (int)((wxListBox*)_obj)->IsSelected(n);
}
	
EWXWEXPORT(void, wxListBox_SetSelection)(void* _obj, int n, int select)
{
	((wxListBox*)_obj)->SetSelection(n, select != 0);
}
	
EWXWEXPORT(int, wxListBox_GetSelection)(void* _obj)
{
	return ((wxListBox*)_obj)->GetSelection();
}
	
EWXWEXPORT(int, wxListBox_GetSelections)(void* _obj, int* aSelections, int allocated)
{
	wxArrayInt sel;
	int result = ((wxListBox*)_obj)->GetSelections(sel);
	
	if (allocated < result) return -result;
	
	for (int i = 0; i < result; i++) aSelections[i] = sel[i];
	return result;
}
	
EWXWEXPORT(void, wxListBox_Append)(void* _obj, wxChar* item)
{
	((wxListBox*)_obj)->Append(item);
}
	
EWXWEXPORT(void, wxListBox_AppendData)(void* _obj, wxChar* item, void* _data)
{
	((wxListBox*)_obj)->Append(item, _data);
}
	
EWXWEXPORT(void, wxListBox_InsertItems)(void* _obj, void* items, int pos, int count)
{
	wxArrayString array;
	
	for (int i = 0; i< count; i++)
		array[i] = ((wxChar**)items)[i];
	
	((wxListBox*)_obj)->InsertItems(array, pos);
}
	
EWXWEXPORT(void, wxListBox_SetFirstItem)(void* _obj, int n)
{
	((wxListBox*)_obj)->SetFirstItem(n);
}
	
EWXWEXPORT(void, wxListBox_SetClientData)(void* _obj, int n, void* clientData)
{
	((wxListBox*)_obj)->SetClientData(n, clientData);
}
	
EWXWEXPORT(void*, wxListBox_GetClientData)(void* _obj, int n)
{
	return (void*)((wxListBox*)_obj)->GetClientData(n);
}
	
EWXWEXPORT(void, wxListBox_SetStringSelection)(void* _obj, wxChar* str, int sel)
{
	((wxListBox*)_obj)->SetStringSelection(str, sel != 0);
}
	
}
