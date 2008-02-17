#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxChoice_Create) (void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _n, void* _str, int _stl)
{
	wxString* list = new wxString[_n];

	for (int i = 0; i < _n; i++)
		list[i] = ((wxChar**)_str)[i];

	wxChoice* result = new wxChoice ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _n, list, _stl, wxDefaultValidator);

	delete [] list;

	return result;
}

EWXWEXPORT(void, wxChoice_Append)(void* _obj, wxChar* item)
{
	((wxChoice*)_obj)->Append(item);
}
	
EWXWEXPORT(void, wxChoice_Delete)(void* _obj, int n)
{
	((wxChoice*)_obj)->Delete(n);
}
	
EWXWEXPORT(void, wxChoice_Clear)(void* _obj)
{
	((wxChoice*)_obj)->Clear();
}
	
EWXWEXPORT(int, wxChoice_GetCount)(void* _obj)
{
	return ((wxChoice*)_obj)->GetCount();
}
	
EWXWEXPORT(int, wxChoice_GetSelection)(void* _obj)
{
	return ((wxChoice*)_obj)->GetSelection();
}
	
EWXWEXPORT(void, wxChoice_SetSelection)(void* _obj, int n)
{
	((wxChoice*)_obj)->SetSelection(n);
}
	
EWXWEXPORT(int, wxChoice_FindString)(void* _obj, wxChar* s)
{
	return ((wxChoice*)_obj)->FindString(s);
}
	
EWXWEXPORT(int, wxChoice_GetString)(void* _obj, int n, void* _buf)
{
	wxString result = ((wxChoice*)_obj)->GetString(n);
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(void, wxChoice_SetString)(void* _obj, int n, wxChar* s)
{
	((wxChoice*)_obj)->SetString(n, s);
}
	
} 
