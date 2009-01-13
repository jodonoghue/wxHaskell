#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxMenuBar_Create)(int _style)
{
	return new wxMenuBar(_style);
}
	
EWXWEXPORT(void,wxMenuBar_DeletePointer)(void* _obj)
{
	delete (wxMenuBar*)_obj;
}
	
EWXWEXPORT(int,wxMenuBar_Append)(wxMenuBar* _obj,wxMenu* menu,wxString* title)
{
	return (int)_obj->Append( menu, *title);
}
	
EWXWEXPORT(int,wxMenuBar_Insert)(wxMenuBar* _obj,int pos,wxMenu* menu,wxString* title)
{
	return (int)_obj->Insert((size_t) pos,  menu, *title);
}
	
EWXWEXPORT(int,wxMenuBar_GetMenuCount)(wxMenuBar* _obj)
{
	return (int)_obj->GetMenuCount();
}
	
EWXWEXPORT(void*,wxMenuBar_GetMenu)(void* _obj,int pos)
{
	return (void*)((wxMenuBar*)_obj)->GetMenu((size_t) pos);
}
	
EWXWEXPORT(void*,wxMenuBar_Replace)(void* _obj,int pos,void* menu,wxString* title)
{
	return (void*)((wxMenuBar*)_obj)->Replace((size_t) pos, (wxMenu*) menu, *title);
}
	
EWXWEXPORT(void*,wxMenuBar_Remove)(void* _obj,int pos)
{
	return (void*)((wxMenuBar*)_obj)->Remove((size_t) pos);
}
	
EWXWEXPORT(void,wxMenuBar_EnableTop)(void* _obj,int pos,int enable)
{
	((wxMenuBar*)_obj)->EnableTop((size_t) pos, enable != 0);
}
	
EWXWEXPORT(void,wxMenuBar_SetLabelTop)(void* _obj,int pos,wxString* label)
{
	((wxMenuBar*)_obj)->SetLabelTop((size_t) pos, *label);
}
	
EWXWEXPORT(wxString*,wxMenuBar_GetLabelTop)(void* _obj,int pos)
{
	wxString *result = new wxString();
	*result = ((wxMenuBar*)_obj)->GetLabelTop((size_t) pos);
	return result;
}
	
EWXWEXPORT(int,wxMenuBar_FindMenuItem)(void* _obj,wxString* menuString,wxString* itemString)
{
	return ((wxMenuBar*)_obj)->FindMenuItem(*menuString, *itemString);
}
	
EWXWEXPORT(void*,wxMenuBar_FindItem)(void* _obj,int id)
{
	wxMenu* _foo = new wxMenu;
	return (void*)((wxMenuBar*)_obj)->FindItem(id, &_foo);
}
	
EWXWEXPORT(int,wxMenuBar_FindMenu)(void* _obj,wxString* title)
{
	return ((wxMenuBar*)_obj)->FindMenu(*title);
}
	
EWXWEXPORT(void,wxMenuBar_EnableItem)(void* _obj,int id,int enable)
{
	((wxMenuBar*)_obj)->Enable(id, enable != 0);
}
	
EWXWEXPORT(void,wxMenuBar_Check)(void* _obj,int id,int check)
{
	((wxMenuBar*)_obj)->Check(id, check != 0);
}
	
EWXWEXPORT(int,wxMenuBar_IsChecked)(wxMenuBar* _obj,int id)
{
	return (int)_obj->IsChecked(id);
}
	
EWXWEXPORT(int,wxMenuBar_IsEnabled)(wxMenuBar* _obj,int id)
{
	return (int)_obj->IsEnabled(id);
}
	
EWXWEXPORT(void,wxMenuBar_SetItemLabel)(void* _obj,int id,wxString* label)
{
	((wxMenuBar*)_obj)->SetLabel(id, *label);
}
	
EWXWEXPORT(wxString*,wxMenuBar_GetLabel)(void* _obj,int id)
{
	wxString *result = new wxString();
	*result = ((wxMenuBar*)_obj)->GetLabel(id);
	return result;
}
	
EWXWEXPORT(void,wxMenuBar_SetHelpString)(void* _obj,int id,wxString* helpString)
{
	((wxMenuBar*)_obj)->SetHelpString(id, *helpString);
}
	
EWXWEXPORT(wxString*,wxMenuBar_GetHelpString)(void* _obj,int id)
{
	wxString *result = new wxString();
	*result = ((wxMenuBar*)_obj)->GetHelpString(id);
	return result;
}
	
EWXWEXPORT(void,wxMenuBar_Enable)(wxMenuBar* _obj,int enable)
{
	_obj->Enable(enable != 0);
}
	
EWXWEXPORT(void,wxMenuBar_SetLabel)(void* _obj,wxString* s)
{
	((wxMenuBar*)_obj)->SetLabel(*s);
}
	
}
