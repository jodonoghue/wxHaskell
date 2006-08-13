#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxFileDialog_Create) (void* _prt, void* _msg, void* _dir, void* _fle, void* _wcd, int _lft, int _top, int _stl)
{
	return (void*) new wxFileDialog ((wxWindow*)_prt, (wxChar*)_msg, (wxChar*) _dir, (wxChar*) _fle, (wxChar*) _wcd, _stl, wxPoint(_lft, _top));
}

EWXWEXPORT(void, wxFileDialog_SetMessage)(void* _obj, void* message)
{
	((wxFileDialog*)_obj)->SetMessage((wxChar*)message);
}
	
EWXWEXPORT(void, wxFileDialog_SetPath)(void* _obj, void* path)
{
	((wxFileDialog*)_obj)->SetPath((wxChar*)path);
}
	
EWXWEXPORT(void, wxFileDialog_SetDirectory)(void* _obj, void* dir)
{
	((wxFileDialog*)_obj)->SetDirectory((wxChar*)dir);
}
	
EWXWEXPORT(void, wxFileDialog_SetFilename)(void* _obj, void* name)
{
	((wxFileDialog*)_obj)->SetFilename((wxChar*)name);
}
	
EWXWEXPORT(void, wxFileDialog_SetWildcard)(void* _obj, void* wildCard)
{
	((wxFileDialog*)_obj)->SetWildcard((wxChar*)wildCard);
}
	
EWXWEXPORT(void, wxFileDialog_SetStyle)(void* _obj, int style)
{
	((wxFileDialog*)_obj)->SetStyle((long)style);
}
	
EWXWEXPORT(void, wxFileDialog_SetFilterIndex)(void* _obj, int filterIndex)
{
	((wxFileDialog*)_obj)->SetFilterIndex(filterIndex);
}
	
EWXWEXPORT(int, wxFileDialog_GetMessage)(void* _obj, void* _buf)
{
	wxString result = ((wxFileDialog*)_obj)->GetMessage();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxFileDialog_GetPath)(void* _obj, void* _buf)
{
	wxString result = ((wxFileDialog*)_obj)->GetPath();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxFileDialog_GetPaths)(void* _obj, void* paths)
{
	wxArrayString arr;
	((wxFileDialog*)_obj)->GetPaths(arr);
	if (paths)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((const wxChar**)paths)[i] = wxStrdup (arr.Item(i).c_str());
	}
	return arr.GetCount();
}
	
EWXWEXPORT(int, wxFileDialog_GetDirectory)(void* _obj, void* _buf)
{
	wxString result =((wxFileDialog*)_obj)->GetDirectory();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxFileDialog_GetFilename)(void* _obj, void* _buf)
{
	wxString result =((wxFileDialog*)_obj)->GetFilename();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxFileDialog_GetFilenames)(void* _obj, void* paths)
{
	wxArrayString arr;
	((wxFileDialog*)_obj)->GetFilenames(arr);
	if (paths)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((const wxChar**)paths)[i] = wxStrdup (arr.Item(i).c_str());
	}
	return arr.GetCount();
}
	
EWXWEXPORT(int, wxFileDialog_GetWildcard)(void* _obj, void* _buf)
{
	wxString result =((wxFileDialog*)_obj)->GetWildcard();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxFileDialog_GetStyle)(void* _obj)
{
	return (int)((wxFileDialog*)_obj)->GetStyle();
}
	
EWXWEXPORT(int, wxFileDialog_GetFilterIndex)(void* _obj)
{
	return ((wxFileDialog*)_obj)->GetFilterIndex();
}
	
}
