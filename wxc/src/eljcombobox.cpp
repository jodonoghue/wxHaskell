#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxComboBox_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _n,void* _str,int _stl)
{
	wxString* list = new wxString[_n];

	for (int i = 0; i < _n; i++)
		list[i] = ((wxChar**)_str)[i];

	wxComboBox* result = new wxComboBox (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _n, list, _stl, wxDefaultValidator);

	delete [] list;
#if wxVERSION_NUMBER < 2400
	if ((result->Number()) && (result->GetSelection() == -1)) result->SetSelection(0);
#else
	if ((result->GetCount()) && (result->GetSelection() == -1)) result->SetSelection(0);
#endif
	
	return (void*) result;
}

EWXWEXPORT(void,wxComboBox_Copy)(wxComboBox* _obj)
{
	_obj->Copy();
}
	
EWXWEXPORT(void,wxComboBox_Cut)(wxComboBox* _obj)
{
	_obj->Cut();
}
	
EWXWEXPORT(void,wxComboBox_Paste)(wxComboBox* _obj)
{
	_obj->Paste();
}
	
EWXWEXPORT(void,wxComboBox_SetInsertionPoint)(wxComboBox* _obj,int pos)
{
	_obj->SetInsertionPoint(pos);
}
	
EWXWEXPORT(void,wxComboBox_SetInsertionPointEnd)(wxComboBox* _obj)
{
	_obj->SetInsertionPointEnd();
}
	
EWXWEXPORT(int,wxComboBox_GetInsertionPoint)(wxComboBox* _obj)
{
	return _obj->GetInsertionPoint();
}
	
EWXWEXPORT(int,wxComboBox_GetLastPosition)(wxComboBox* _obj)
{
	return _obj->GetLastPosition();
}
	
EWXWEXPORT(void,wxComboBox_Replace)(wxComboBox* _obj,int from,int to,wxString* value)
{
	_obj->Replace(from, to, *value);
}
	
EWXWEXPORT(void,wxComboBox_Remove)(wxComboBox* _obj,int from,int to)
{
	_obj->Remove(from, to);
#if wxVERSION_NUMBER < 2400
	if ((_obj->Number()) && (_obj->GetSelection() == -1)) _obj->SetSelection(0);
#else
	if ((_obj->GetCount()) && (_obj->GetSelection() == -1)) _obj->SetSelection(0);
#endif
}
	
EWXWEXPORT(void,wxComboBox_SetTextSelection)(wxComboBox* _obj,int from,int to)
{
	_obj->SetSelection(from, to);
}
	
EWXWEXPORT(void,wxComboBox_SetEditable)(wxComboBox* _obj,int editable)
{
	_obj->SetEditable(editable != 0);
}
	
EWXWEXPORT(wxString*,wxComboBox_GetStringSelection)(wxComboBox* _obj)
{
	wxString *result = new wxString();
	*result = _obj->GetStringSelection();
	return result;
}
	
EWXWEXPORT(wxString*,wxComboBox_GetValue)(wxComboBox* _obj)
{
	wxString *result = new wxString();
	*result = _obj->GetValue();
	return result;
}
	
EWXWEXPORT(void,wxComboBox_Append)(wxComboBox* _obj,wxString* item)
{
	_obj->Append(*item);
#if wxVERSION_NUMBER < 2400
	if (_obj->Number() && (_obj->GetSelection() == -1)) _obj->SetSelection(0);
#else
	if (_obj->GetCount() && (_obj->GetSelection() == -1)) _obj->SetSelection(0);
#endif
}
	
EWXWEXPORT(void,wxComboBox_AppendData)(wxComboBox* _obj,wxString* item,void* d)
{
#if defined(__WXMAC__)
    _obj->Append(*item);
#else
    _obj->Append(*item, d);
#endif

#if wxVERSION_NUMBER < 2400
	if ((_obj->Number()) && (_obj->GetSelection() == -1)) _obj->SetSelection(0);
#else
	if ((_obj->GetCount()) && (_obj->GetSelection() == -1)) _obj->SetSelection(0);
#endif
}
	
EWXWEXPORT(void,wxComboBox_Delete)(wxComboBox* _obj,int n)
{
	_obj->Delete(n);
#if wxVERSION_NUMBER < 2400
	if ((_obj->Number()) && (_obj->GetSelection() == -1)) _obj->SetSelection(0);
#else
	if ((_obj->GetCount()) && (_obj->GetSelection() == -1)) _obj->SetSelection(0);
#endif
}
	
EWXWEXPORT(void,wxComboBox_Clear)(wxComboBox* _obj)
{
	_obj->Clear();
}
	
EWXWEXPORT(int,wxComboBox_GetCount)(wxComboBox* _obj)
{
#if wxVERSION_NUMBER < 2400
	return _obj->Number();
#else
	return _obj->GetCount();
#endif
}
	
EWXWEXPORT(int,wxComboBox_GetSelection)(wxComboBox* _obj)
{
	return _obj->GetSelection();
}
	
EWXWEXPORT(void,wxComboBox_SetSelection)(wxComboBox* _obj,int n)
{
	_obj->SetSelection(n);
}
	
EWXWEXPORT(int,wxComboBox_FindString)(wxComboBox* _obj,wxString* s)
{
	return _obj->FindString(*s);
}
	
EWXWEXPORT(wxString*,wxComboBox_GetString)(wxComboBox* _obj,int n)
{
	wxString *result = new wxString();
	*result = _obj->GetString(n);
	return result;
}
	
EWXWEXPORT(void,wxComboBox_SetString)(wxComboBox* _obj,int n,wxString* s)
{
	_obj->SetString(n,*s);
}
	
EWXWEXPORT(void,wxComboBox_SetClientData)(wxComboBox* _obj,int n,void* clientData)
{
	_obj->SetClientData( n, clientData );
}
	
EWXWEXPORT(void*,wxComboBox_GetClientData)(wxComboBox* _obj,int n)
{
	return _obj->GetClientData(n);
}

}
