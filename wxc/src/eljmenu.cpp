#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxMenu_Create)(wxString* title,long style)
{
	return (void*) new wxMenu(*title, style);
}
	
EWXWEXPORT(void,wxMenu_DeletePointer)(void* _obj)
{
	delete (wxMenu*)_obj;
}
	
EWXWEXPORT(void,wxMenu_AppendSeparator)(void* _obj)
{
	((wxMenu*)_obj)->AppendSeparator();
}
	
EWXWEXPORT(void,wxMenu_Append)(void* _obj,int id,wxString* text,wxString* help,int isCheckable)
{
	((wxMenu*)_obj)->Append(id, *text, *help, isCheckable != 0);
}
	
EWXWEXPORT(void,wxMenu_AppendSub)(void* _obj,int id,wxString* text,void* submenu,wxString* help)
{
	((wxMenu*)_obj)->Append(id, *text, (wxMenu*) submenu, *help);
}
	
EWXWEXPORT(void,wxMenu_AppendItem)(void* _obj,void* _itm)
{
	((wxMenu*)_obj)->Append((wxMenuItem*)_itm);
}
	
EWXWEXPORT(void,wxMenu_Break)(void* _obj)
{
	((wxMenu*)_obj)->Break();
}
	
EWXWEXPORT(void,wxMenu_Insert)(void* _obj,size_t pos,int id,wxString* text,wxString* help,int isCheckable)
{
	((wxMenu*)_obj)->Insert(pos, id, *text, *help, isCheckable != 0);
}
	
EWXWEXPORT(void,wxMenu_InsertSub)(void* _obj,size_t pos,int id,wxString* text,void* submenu,wxString* help)
{
	((wxMenu*)_obj)->Insert(pos, id, *text, (wxMenu*) submenu, *help);
}
	
EWXWEXPORT(void,wxMenu_InsertItem)(void* _obj,int pos,void* _itm)
{
	((wxMenu*)_obj)->Insert((size_t)pos, (wxMenuItem*)_itm);
}
	
EWXWEXPORT(void,wxMenu_Prepend)(void* _obj,int id,wxString* text,wxString* help,int isCheckable)
{
	((wxMenu*)_obj)->Prepend(id, *text, *help, isCheckable != 0);
}
	
EWXWEXPORT(void,wxMenu_PrependSub)(void* _obj,int id,wxString* text,void* submenu,wxString* help)
{
	((wxMenu*)_obj)->Prepend(id, *text, (wxMenu*) submenu, *help);
}
	
EWXWEXPORT(void,wxMenu_PrependItem)(void* _obj,void* _itm)
{
	((wxMenu*)_obj)->Prepend((wxMenuItem*)_itm);
}
	
EWXWEXPORT(void,wxMenu_RemoveByItem)(void* _obj,void* item)
{
	((wxMenu*)_obj)->Remove((wxMenuItem*) item);
}
	
EWXWEXPORT(void,wxMenu_RemoveById)(void* _obj,int id,void* _itm)
{
	*((void**)_itm) = (void*)((wxMenu*)_obj)->Remove(id);
}
	
EWXWEXPORT(void,wxMenu_DeleteById)(void* _obj,int id)
{
	((wxMenu*)_obj)->Delete(id);
}
	
EWXWEXPORT(void,wxMenu_DeleteByItem)(void* _obj,void* _itm)
{
	((wxMenu*)_obj)->Delete((wxMenuItem*)_itm);
}
	
EWXWEXPORT(void,wxMenu_DestroyById)(void* _obj,int id)
{
	((wxMenu*)_obj)->Destroy(id);
}
	
EWXWEXPORT(void,wxMenu_DestroyByItem)(void* _obj,void* _itm)
{
	((wxMenu*)_obj)->Destroy((wxMenuItem*)_itm);
}
	
EWXWEXPORT(size_t,wxMenu_GetMenuItemCount)(void* _obj)
{
	return ((wxMenu*)_obj)->GetMenuItemCount();
}
	
EWXWEXPORT(int,wxMenu_GetMenuItems)(void* _obj,void* _lst)
{
	if (_lst)
	{
		for (unsigned int i = 0; i < ((wxMenu*)_obj)->GetMenuItems().GetCount(); i++)
			((void**)_lst)[i] = ((wxMenu*)_obj)->GetMenuItems().Item(i)->GetData();
	}
	return ((wxMenu*)_obj)->GetMenuItems().GetCount();
}
	
EWXWEXPORT(int,wxMenu_FindItemByLabel)(void* _obj,wxString* itemString)
{
	return ((wxMenu*)_obj)->FindItem(*itemString);
}
	
EWXWEXPORT(void*,wxMenu_FindItem)(void* _obj,int id)
{
	wxMenu* _foo = new wxMenu;
	return (void*)((wxMenu*)_obj)->FindItem(id, &_foo);
}
	
EWXWEXPORT(void,wxMenu_Enable)(void* _obj,int id,int enable)
{
	((wxMenu*)_obj)->Enable(id, enable != 0);
}
	
EWXWEXPORT(int,wxMenu_IsEnabled)(wxMenu* _obj,int id)
{
	return (int)_obj->IsEnabled(id);
}
	
EWXWEXPORT(void,wxMenu_Check)(void* _obj,int id,int check)
{
	((wxMenu*)_obj)->Check(id, check != 0);
}
	
EWXWEXPORT(int,wxMenu_IsChecked)(wxMenu* _obj,int id)
{
	return (int)_obj->IsChecked(id);
}
	
EWXWEXPORT(void,wxMenu_SetLabel)(void* _obj,int id,wxString* label)
{
	((wxMenu*)_obj)->SetLabel(id, *label);
}
	
EWXWEXPORT(wxString*,wxMenu_GetLabel)(void* _obj,int id)
{
	wxString *result = new wxString();
	*result = ((wxMenu*)_obj)->GetLabel(id);
	return result;
}
	
EWXWEXPORT(void,wxMenu_SetHelpString)(void* _obj,int id,wxString* helpString)
{
	((wxMenu*)_obj)->SetHelpString(id, *helpString);
}
	
EWXWEXPORT(wxString*,wxMenu_GetHelpString)(void* _obj,int id)
{
	wxString *result = new wxString();
	*result = ((wxMenu*)_obj)->GetHelpString(id);
	return result;
}
	
EWXWEXPORT(void,wxMenu_SetTitle)(void* _obj,wxString* title)
{
	((wxMenu*)_obj)->SetTitle(*title);
}
	
EWXWEXPORT(wxString*,wxMenu_GetTitle)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxMenu*)_obj)->GetTitle();
	return result;
}
	
EWXWEXPORT(void,wxMenu_SetClientData)(void* _obj,void* clientData)
{
	((wxMenu*)_obj)->SetClientData(clientData);
}
	
EWXWEXPORT(void*,wxMenu_GetClientData)(void* _obj)
{
	return (void*)((wxMenu*)_obj)->GetClientData();
}
	
EWXWEXPORT(void,wxMenu_SetEventHandler)(void* _obj,void* handler)
{
	((wxMenu*)_obj)->SetEventHandler((wxEvtHandler*) handler);
}
	
EWXWEXPORT(void,wxMenu_SetInvokingWindow)(void* _obj,wxWindow* win)
{
	((wxMenu*)_obj)->SetInvokingWindow(win);
}
	
EWXWEXPORT(void*,wxMenu_GetInvokingWindow)(void* _obj)
{
	return (void*)((wxMenu*)_obj)->GetInvokingWindow();
}
	
EWXWEXPORT(int,wxMenu_GetStyle)(void* _obj)
{
	return ((wxMenu*)_obj)->GetStyle();
}
	
EWXWEXPORT(void,wxMenu_UpdateUI)(void* _obj,void* source)
{
	((wxMenu*)_obj)->UpdateUI((wxEvtHandler*) source);
}
	
EWXWEXPORT(int,wxMenu_IsAttached)(wxMenu* _obj)
{
	return (int)_obj->IsAttached();
}
	
EWXWEXPORT(void,wxMenu_SetParent)(void* _obj,void* parent)
{
	((wxMenu*)_obj)->SetParent((wxMenu*) parent);
}
	
EWXWEXPORT(void*,wxMenu_GetParent)(void* _obj)
{
	return (void*)((wxMenu*)_obj)->GetParent();
}
	

EWXWEXPORT(void*,wxMenuItem_Create)()
{
	return (void*) new wxMenuItem();
}
	
EWXWEXPORT(void,wxMenuItem_Delete)(void* _obj)
{
	delete (wxMenuItem*)_obj;
}

EWXWEXPORT(void*,wxMenuItem_GetMenu)(void* _obj)
{
	return (void*)((wxMenuItem*)_obj)->GetMenu();
}
	
EWXWEXPORT(void,wxMenuItem_SetId)(void* _obj,int id)
{
	((wxMenuItem*)_obj)->SetId(id);
}
	
EWXWEXPORT(int,wxMenuItem_GetId)(void* _obj)
{
	return ((wxMenuItem*)_obj)->GetId();
}
	
EWXWEXPORT(int,wxMenuItem_IsSeparator)(wxMenuItem* _obj)
{
	return (int)_obj->IsSeparator();
}
	
EWXWEXPORT(void,wxMenuItem_SetText)(void* _obj,wxString* str)
{
	((wxMenuItem*)_obj)->SetText(* str);
}
	
EWXWEXPORT(wxString*,wxMenuItem_GetLabel)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxMenuItem*)_obj)->GetLabel();
	return result;
}
	
EWXWEXPORT(wxString*,wxMenuItem_GetText)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxMenuItem*)_obj)->GetText();
	return result;
}

EWXWEXPORT(wxString*,wxMenuItem_GetLabelFromText)(wxString* text)
{
	wxString *result = new wxString();
	*result = wxMenuItem::GetLabelFromText(* text);
	return result;
}

EWXWEXPORT(void,wxMenuItem_SetCheckable)(void* _obj,int checkable)
{
	((wxMenuItem*)_obj)->SetCheckable(checkable != 0);
}
	
EWXWEXPORT(int,wxMenuItem_IsCheckable)(wxMenuItem* _obj)
{
	return (int)_obj->IsCheckable();
}
	
EWXWEXPORT(int,wxMenuItem_IsSubMenu)(wxMenuItem* _obj)
{
	return (int)_obj->IsSubMenu();
}
	
EWXWEXPORT(void,wxMenuItem_SetSubMenu)(void* _obj,void* menu)
{
	((wxMenuItem*)_obj)->SetSubMenu((wxMenu*)menu);
}
	
EWXWEXPORT(void*,wxMenuItem_GetSubMenu)(void* _obj)
{
	return (void*)((wxMenuItem*)_obj)->GetSubMenu();
}
	
EWXWEXPORT(void,wxMenuItem_Enable)(void* _obj,int enable)
{
	((wxMenuItem*)_obj)->Enable(enable != 0);
}
	
EWXWEXPORT(int,wxMenuItem_IsEnabled)(wxMenuItem* _obj)
{
	return (int) _obj->IsEnabled();
}
	
EWXWEXPORT(void,wxMenuItem_Check)(void* _obj,int check)
{
	((wxMenuItem*)_obj)->Check(check != 0);
}
	
EWXWEXPORT(int,wxMenuItem_IsChecked)(wxMenuItem* _obj)
{
	return (int)_obj->IsChecked();
}
	
EWXWEXPORT(void,wxMenuItem_SetHelp)(void* _obj,wxString* str)
{
	((wxMenuItem*)_obj)->SetHelp(*str);
}
	
EWXWEXPORT(wxString*,wxMenuItem_GetHelp)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxMenuItem*)_obj)->GetHelp();
	return result;
}

}
