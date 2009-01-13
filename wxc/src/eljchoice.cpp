#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxChoice_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _n,void* _str,int _stl)
{
	wxString* list = new wxString[_n];

	for (int i = 0; i < _n; i++)
		list[i] = ((wxChar**)_str)[i];

	wxChoice* result = new wxChoice (_prt, _id, wxPoint(_lft, _top),wxSize(_wdt, _hgt), _n, list, _stl, wxDefaultValidator);

	delete [] list;

	return result;
}

EWXWEXPORT(void,wxChoice_Append)(wxChoice* _obj,wxString* item)
{
	_obj->Append(*item);
}
	
EWXWEXPORT(void,wxChoice_Delete)(wxChoice* _obj,int n)
{
	_obj->Delete(n);
}
	
EWXWEXPORT(void,wxChoice_Clear)(wxChoice* _obj)
{
	_obj->Clear();
}
	
EWXWEXPORT(int,wxChoice_GetCount)(wxChoice* _obj)
{
	return _obj->GetCount();
}
	
EWXWEXPORT(int,wxChoice_GetSelection)(wxChoice* _obj)
{
	return _obj->GetSelection();
}
	
EWXWEXPORT(void,wxChoice_SetSelection)(wxChoice* _obj,int n)
{
	_obj->SetSelection(n);
}
	
EWXWEXPORT(int,wxChoice_FindString)(wxChoice* _obj,wxString* s)
{
	return _obj->FindString(*s);
}
	
EWXWEXPORT(wxString*,wxChoice_GetString)(wxChoice* _obj,int n)
{
	wxString *result = new wxString();
	*result = _obj->GetString(n);
	return result;
}
	
EWXWEXPORT(void,wxChoice_SetString)(wxChoice* _obj,int n,wxString* s)
{
	_obj->SetString(n, *s);
}
	
} 
