#include "wrapper.h"
#include "wx/docview.h"

extern "C"
{

EWXWEXPORT(void*,wxFileHistory_Create)(int maxFiles)
{
	return (void*)new wxFileHistory(maxFiles);
}
	
EWXWEXPORT(void,wxFileHistory_Delete)(void* _obj)
{
	delete (wxFileHistory*)_obj;
}
	
EWXWEXPORT(void,wxFileHistory_AddFileToHistory)(void* _obj, void* file)
{
	((wxFileHistory*)_obj)->AddFileToHistory((const char*)file);
}
	
EWXWEXPORT(void,wxFileHistory_RemoveFileFromHistory)(void* _obj, int i)
{
	((wxFileHistory*)_obj)->RemoveFileFromHistory(i);
}
	
EWXWEXPORT(int,wxFileHistory_GetMaxFiles)(void* _obj)
{
	return ((wxFileHistory*)_obj)->GetMaxFiles();
}
	
EWXWEXPORT(void,wxFileHistory_UseMenu)(void* _obj, void* menu)
{
	((wxFileHistory*)_obj)->UseMenu((wxMenu*)menu);
}
	
EWXWEXPORT(void,wxFileHistory_RemoveMenu)(void* _obj, void* menu)
{
	((wxFileHistory*)_obj)->RemoveMenu((wxMenu*)menu);
}
	
EWXWEXPORT(void,wxFileHistory_Load)(void* _obj, void* config)
{
	((wxFileHistory*)_obj)->Load(*((wxConfigBase*)config));
}
	
EWXWEXPORT(void,wxFileHistory_Save)(void* _obj, void* config)
{
	((wxFileHistory*)_obj)->Save(*((wxConfigBase*)config));
}
	
EWXWEXPORT(void,wxFileHistory_AddFilesToMenu)(void* _obj, void* menu)
{
	if (menu)
		((wxFileHistory*)_obj)->AddFilesToMenu((wxMenu*)menu);
	else
		((wxFileHistory*)_obj)->AddFilesToMenu();
}
	
EWXWEXPORT(int,wxFileHistory_GetHistoryFile)(void* _obj, int i, void* _buf)
{
	wxString tmp = ((wxFileHistory*)_obj)->GetHistoryFile(i);
	if (_buf) memcpy (_buf, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int,wxFileHistory_GetCount)(void* _obj)
{
	return ((wxFileHistory*)_obj)->GetCount();
}
	
EWXWEXPORT(int,wxFileHistory_GetNoHistoryFiles)(void* _obj)
{
	return ((wxFileHistory*)_obj)->GetNoHistoryFiles();
}
	
EWXWEXPORT(int,wxFileHistory_GetMenus)(void* _obj, void* _ref)
{
	wxList lst = ((wxFileHistory*)_obj)->GetMenus();
	if (_ref)
	{
		for (unsigned int i = 0; i < lst.GetCount(); i++)
			((void**)_ref)[i] = (void*)lst.Item(i);
	}
	
	return lst.GetCount();
}
	
}
