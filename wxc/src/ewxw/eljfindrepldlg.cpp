#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/fdrepdlg.h"

extern "C"
{

EWXWEXPORT(void*,wxFindReplaceData_CreateDefault)()
{
	return (void*)new wxFindReplaceData();
}
	
EWXWEXPORT(void*,wxFindReplaceData_Create)(int flags)
{
	return (void*)new wxFindReplaceData((wxUint32)flags);
}

EWXWEXPORT(void,wxFindReplaceData_Delete)(void* _obj)
{
	delete (wxFindReplaceData*)_obj;
}
	
EWXWEXPORT(int,wxFindReplaceData_GetFindString)(void* _obj, void* _ref)
{
	wxString tmp = ((wxFindReplaceData*)_obj)->GetFindString();
	if (_ref)
		memcpy(_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int,wxFindReplaceData_GetReplaceString)(void* _obj, void* _ref)
{
	wxString tmp = ((wxFindReplaceData*)_obj)->GetFindString();
	if (_ref)
		memcpy(_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int,wxFindReplaceData_GetFlags)(void* _obj)
{
	return ((wxFindReplaceData*)_obj)->GetFlags();
}
	
EWXWEXPORT(void,wxFindReplaceData_SetFlags)(void* _obj, int flags)
{
	((wxFindReplaceData*)_obj)->SetFlags((wxUint32)flags);
}
	
EWXWEXPORT(void,wxFindReplaceData_SetFindString)(void* _obj, void* str)
{
	((wxFindReplaceData*)_obj)->SetFindString((char*)str);
}
	
EWXWEXPORT(void,wxFindReplaceData_SetReplaceString)(void* _obj, void* str)
{
	((wxFindReplaceData*)_obj)->SetReplaceString((char*)str);
}
	

EWXWEXPORT(int,wxFindDialogEvent_GetFlags)(void* _obj)
{
	return ((wxFindDialogEvent*)_obj)->GetFlags();
}
	
EWXWEXPORT(int,wxFindDialogEvent_GetFindString)(void* _obj, void* _ref)
{
	wxString tmp = ((wxFindReplaceData*)_obj)->GetFindString();
	if (_ref)
		memcpy(_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int,wxFindDialogEvent_GetReplaceString)(void* _obj, void* _ref)
{
	wxString tmp = ((wxFindReplaceData*)_obj)->GetFindString();
	if (_ref)
		memcpy(_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	

EWXWEXPORT(void*,wxFindReplaceDialog_Create)(void* parent, void* data, void* title, int style)
{
	return (void*)new wxFindReplaceDialog((wxWindow*)parent, (wxFindReplaceData*)data, (char*)title, style);
}
	
EWXWEXPORT(void*,wxFindReplaceDialog_GetData)(void* _obj)
{
	return (void*)((wxFindReplaceDialog*)_obj)->GetData();
}
	
EWXWEXPORT(void,wxFindReplaceDialog_SetData)(void* _obj, void* data)
{
	((wxFindReplaceDialog*)_obj)->SetData((wxFindReplaceData*)data);
}
	

EWXWEXPORT(int,expEVT_COMMAND_FIND)()
{
	return (int)wxEVT_COMMAND_FIND;
}

EWXWEXPORT(int,expEVT_COMMAND_FIND_NEXT)()
{
	return (int)wxEVT_COMMAND_FIND_NEXT;
}

EWXWEXPORT(int,expEVT_COMMAND_FIND_REPLACE)()
{
	return (int)wxEVT_COMMAND_FIND_REPLACE;
}

EWXWEXPORT(int,expEVT_COMMAND_FIND_REPLACE_ALL)()
{
	return (int)wxEVT_COMMAND_FIND_REPLACE_ALL;
}

EWXWEXPORT(int,expEVT_COMMAND_FIND_CLOSE)()
{
	return (int)wxEVT_COMMAND_FIND_CLOSE;
}

}
#endif
