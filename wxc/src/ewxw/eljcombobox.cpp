#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxComboBox_Create) (void* _prt, int _id, char* _txt, int _lft, int _top, int _wdt, int _hgt, int _n, void* _str, int _stl)
{
	wxString* list = new wxString[_n];

	for (int i = 0; i < _n; i++)
		list[i] = ((char**)_str)[i];

	wxComboBox* result = new wxComboBox ((wxWindow*)_prt, _id, _txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _n, list, _stl, wxDefaultValidator);

	delete [] list;
#if wxVERSION_NUMBER < 2400
	if ((result->Number()) && (result->GetSelection() == -1)) result->SetSelection(0);
#else
	if ((result->GetCount()) && (result->GetSelection() == -1)) result->SetSelection(0);
#endif
	
	return (void*) result;
}

EWXWEXPORT(void, wxComboBox_Copy)(void* _obj)
{
	((wxComboBox*)_obj)->Copy();
}
	
EWXWEXPORT(void, wxComboBox_Cut)(void* _obj)
{
	((wxComboBox*)_obj)->Cut();
}
	
EWXWEXPORT(void, wxComboBox_Paste)(void* _obj)
{
	((wxComboBox*)_obj)->Paste();
}
	
EWXWEXPORT(void, wxComboBox_SetInsertionPoint)(void* _obj, int pos)
{
	((wxComboBox*)_obj)->SetInsertionPoint(pos);
}
	
EWXWEXPORT(void, wxComboBox_SetInsertionPointEnd)(void* _obj)
{
	((wxComboBox*)_obj)->SetInsertionPointEnd();
}
	
EWXWEXPORT(int, wxComboBox_GetInsertionPoint)(void* _obj)
{
	return ((wxComboBox*)_obj)->GetInsertionPoint();
}
	
EWXWEXPORT(int, wxComboBox_GetLastPosition)(void* _obj)
{
	return ((wxComboBox*)_obj)->GetLastPosition();
}
	
EWXWEXPORT(void, wxComboBox_Replace)(void* _obj, int from, int to, char* value)
{
	((wxComboBox*)_obj)->Replace(from, to, value);
}
	
EWXWEXPORT(void, wxComboBox_Remove)(void* _obj, int from, int to)
{
	((wxComboBox*)_obj)->Remove(from, to);
#if wxVERSION_NUMBER < 2400
	if ((((wxComboBox*)_obj)->Number()) && (((wxComboBox*)_obj)->GetSelection() == -1)) ((wxComboBox*)_obj)->SetSelection(0);
#else
	if ((((wxComboBox*)_obj)->GetCount()) && (((wxComboBox*)_obj)->GetSelection() == -1)) ((wxComboBox*)_obj)->SetSelection(0);
#endif
}
	
EWXWEXPORT(void, wxComboBox_SetTextSelection)(void* _obj, int from, int to)
{
	((wxComboBox*)_obj)->SetSelection(from, to);
}
	
EWXWEXPORT(void, wxComboBox_SetEditable)(void* _obj, int editable)
{
	((wxComboBox*)_obj)->SetEditable(editable != 0);
}
	
EWXWEXPORT(int, wxComboBox_GetStringSelection)(void* _obj, void* _buf)
{
	wxString result = ((wxComboBox*)_obj)->GetStringSelection();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(int, wxComboBox_GetValue)(void* _obj, void* _buf)
{
	wxString result = ((wxComboBox*)_obj)->GetValue();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(void, wxComboBox_Append)(void* _obj, char* item)
{
	((wxComboBox*)_obj)->Append(item);
#if wxVERSION_NUMBER < 2400
	if ((((wxComboBox*)_obj)->Number()) && (((wxComboBox*)_obj)->GetSelection() == -1)) ((wxComboBox*)_obj)->SetSelection(0);
#else
	if ((((wxComboBox*)_obj)->GetCount()) && (((wxComboBox*)_obj)->GetSelection() == -1)) ((wxComboBox*)_obj)->SetSelection(0);
#endif
}
	
EWXWEXPORT(void, wxComboBox_AppendData)(void* _obj, char* item, void* d)
{
#if defined(__WXMAC__)
    ((wxComboBox*)_obj)->Append(item);
#else
    ((wxComboBox*)_obj)->Append(item, d);
#endif

#if wxVERSION_NUMBER < 2400
	if ((((wxComboBox*)_obj)->Number()) && (((wxComboBox*)_obj)->GetSelection() == -1)) ((wxComboBox*)_obj)->SetSelection(0);
#else
	if ((((wxComboBox*)_obj)->GetCount()) && (((wxComboBox*)_obj)->GetSelection() == -1)) ((wxComboBox*)_obj)->SetSelection(0);
#endif
}
	
EWXWEXPORT(void, wxComboBox_Delete)(void* _obj, int n)
{
	((wxComboBox*)_obj)->Delete(n);
#if wxVERSION_NUMBER < 2400
	if ((((wxComboBox*)_obj)->Number()) && (((wxComboBox*)_obj)->GetSelection() == -1)) ((wxComboBox*)_obj)->SetSelection(0);
#else
	if ((((wxComboBox*)_obj)->GetCount()) && (((wxComboBox*)_obj)->GetSelection() == -1)) ((wxComboBox*)_obj)->SetSelection(0);
#endif
}
	
EWXWEXPORT(void, wxComboBox_Clear)(void* _obj)
{
	((wxComboBox*)_obj)->Clear();
}
	
EWXWEXPORT(int, wxComboBox_GetCount)(void* _obj)
{
#if wxVERSION_NUMBER < 2400
	return ((wxComboBox*)_obj)->Number();
#else
	return ((wxComboBox*)_obj)->GetCount();
#endif
}
	
EWXWEXPORT(int, wxComboBox_GetSelection)(void* _obj)
{
	return ((wxComboBox*)_obj)->GetSelection();
}
	
EWXWEXPORT(void, wxComboBox_SetSelection)(void* _obj, int n)
{
	((wxComboBox*)_obj)->SetSelection(n);
}
	
EWXWEXPORT(int, wxComboBox_FindString)(void* _obj, char* s)
{
	return ((wxComboBox*)_obj)->FindString(s);
}
	
EWXWEXPORT(int, wxComboBox_GetString)(void* _obj, int n, void* _buf)
{
	wxString result = ((wxComboBox*)_obj)->GetString(n);
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(void, wxComboBox_SetString)(void* _obj, int n, char* s)
{
}
	
EWXWEXPORT(void, wxComboBox_SetClientData)(void* _obj, int n, void* clientData)
{
	((wxComboBox*)_obj)->SetClientData( n, clientData );
}
	
EWXWEXPORT(void*, wxComboBox_GetClientData)(void* _obj, int n)
{
	return ((wxComboBox*)_obj)->GetClientData(n);
}

}
