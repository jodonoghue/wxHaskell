#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxValidator_Create)()
{
	return (void*) new wxValidator();
}

EWXWEXPORT(void, wxValidator_Delete)(void* _obj)
{
	delete (wxValidator*)_obj;
}

EWXWEXPORT(int, wxValidator_Validate)(void* _obj, void* parent)
{
	return (int)((wxValidator*)_obj)->Validate((wxWindow*)parent);
}
	
EWXWEXPORT(int, wxValidator_TransferToWindow)(void* _obj)
{
	return (int)((wxValidator*)_obj)->TransferToWindow();
}
	
EWXWEXPORT(int, wxValidator_TransferFromWindow)(void* _obj)
{
	return (int)((wxValidator*)_obj)->TransferFromWindow();
}
	
EWXWEXPORT(void*, wxValidator_GetWindow)(void* _obj)
{
	return (void*)((wxValidator*)_obj)->GetWindow();
}
	
EWXWEXPORT(void, wxValidator_SetWindow)(void* _obj, void* win)
{
	((wxValidator*)_obj)->SetWindow((wxWindowBase*)win);
}
	
EWXWEXPORT(int, wxValidator_IsSilent)()
{
	return (int)wxValidator::IsSilent();
}
	
EWXWEXPORT(void, wxValidator_SetBellOnError)(int doIt)
{
	wxValidator::SetBellOnError(doIt != 0);
}
	
EWXWEXPORT(void*, wxTextValidator_Create)(int style, void* val)
{
	return (void*) new wxTextValidator((long)style, new wxString);
}

EWXWEXPORT(int, wxTextValidator_GetStyle)(void* _obj)
{
	return (int)((wxTextValidator*)_obj)->GetStyle();
}
	
EWXWEXPORT(void, wxTextValidator_SetStyle)(void* _obj, int style)
{
	((wxTextValidator*)_obj)->SetStyle((long) style);
}
	
EWXWEXPORT(void, wxTextValidator_SetIncludeList)(void* _obj, void* list, int count)
{
	wxStringList str;
	
	for (int i = 0; i < count; i++)
		str.Add(((char**)list)[i]);
		
	((wxTextValidator*)_obj)->SetIncludeList(str);
}
	
EWXWEXPORT(int, wxTextValidator_GetIncludeList)(void* _obj, void* _ref)
{
/*
	if (_ref)
	{
		for (unsigned int i = 0; i < ((wxTextValidator*)_obj)->GetIncludeList().GetCount(); i++)
			((const char**)_ref)[i] = strdup(((wxTextValidator*)_obj)->GetIncludeList().Item(i)->GetData());
	}
	return ((wxTextValidator*)_obj)->GetIncludeList().GetCount();
*/
        return 0;
}
	
EWXWEXPORT(void, wxTextValidator_SetExcludeList)(void* _obj, void* list, int count)
{
	wxStringList str;
	
	for (int i = 0; i < count; i++)
		str.Add(((char**)list)[i]);
		
	((wxTextValidator*)_obj)->SetExcludeList(str);
}
	
EWXWEXPORT(int, wxTextValidator_GetExcludeList)(void* _obj, void* _ref)
{
/*
	if (_ref)
	{
		for (unsigned int i = 0; i < ((wxTextValidator*)_obj)->GetExcludeList().GetCount(); i++)
			((const char**)_ref)[i] = ((wxTextValidator*)_obj)->GetExcludeList().Item(i)->GetData();
	}
	return ((wxTextValidator*)_obj)->GetExcludeList().GetCount();
*/
	return 0;
}
	
EWXWEXPORT(void, wxTextValidator_OnChar)(void* _obj, void* event)
{
	((wxTextValidator*)_obj)->OnChar(*((wxKeyEvent*)event));
}

EWXWEXPORT(void*, ELJTextValidator_Create) (void* _obj, void* _fnc, void* _txt, long _stl)
{
	return new ELJTextValidator(_obj, _fnc, _txt, _stl);
}

}

bool ELJTextValidator::Validate(wxWindow* _prt)
{
	if (obj && fnc)
		return fnc(obj) != 0;
	else
		return wxTextValidator::Validate(_prt);
}
