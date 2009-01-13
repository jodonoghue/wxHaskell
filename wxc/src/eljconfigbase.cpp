#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxConfigBase_Create)()
{
	return (void*) wxConfigBase::Create();
}
	
EWXWEXPORT(void,wxConfigBase_Delete)(void* _obj)
{
	delete (wxConfigBase*)_obj;
}

EWXWEXPORT(void,wxConfigBase_SetPath)(void* _obj,wxString* strPath)
{
	((wxConfigBase*)_obj)->SetPath(*strPath);
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetPath)(wxString* _obj)
{
	wxString *result = new wxString();
	*result = ((wxConfigBase*)_obj)->GetPath();
	return result;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetFirstGroup)(wxConfigBase* _obj,long* lIndex)
{
	wxString* tmp;
        tmp = new wxString(wxT(""));
        if (_obj->GetFirstGroup(*tmp, *lIndex)) {
          *lIndex = -1;
        }         
	return tmp;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetNextGroup)(wxConfigBase* _obj,long* lIndex)
{
	wxString* tmp;
        tmp = new wxString(wxT(""));
        if (_obj->GetNextGroup(*tmp, *lIndex)) {
          *lIndex = -1;
        }         
	return tmp;
}

	
EWXWEXPORT(wxString*,wxConfigBase_GetFirstEntry)(wxConfigBase* _obj,long* lIndex)
{
	wxString* tmp;
        tmp = new wxString(wxT(""));
        if (_obj->GetFirstEntry(*tmp, *lIndex)) {
          *lIndex = -1;
        }         
	return tmp;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetNextEntry)(wxConfigBase* _obj,long* lIndex)
{
	wxString* tmp;
        tmp = new wxString(wxT(""));
        if (_obj->GetNextEntry(*tmp, *lIndex)) {
          *lIndex = -1;
        }         
	return tmp;
}
	
EWXWEXPORT(int,wxConfigBase_GetNumberOfEntries)(wxConfigBase* _obj,int bRecursive)
{
	return (int)_obj->GetNumberOfEntries(bRecursive != 0);
}
	
EWXWEXPORT(int,wxConfigBase_GetNumberOfGroups)(wxConfigBase* _obj,int bRecursive)
{
	return (int)_obj->GetNumberOfGroups(bRecursive != 0);
}
	
EWXWEXPORT(int,wxConfigBase_HasGroup)(wxConfigBase* _obj,wxString* strName)
{
	return (int)_obj->HasGroup(*strName);
}
	
EWXWEXPORT(int,wxConfigBase_HasEntry)(wxConfigBase* _obj,wxString* strName)
{
	return (int)_obj->HasEntry(*strName);
}
	
EWXWEXPORT(int,wxConfigBase_Exists)(wxConfigBase* _obj,wxString* strName)
{
	return (int)_obj->Exists(*strName);
}
	
EWXWEXPORT(int,wxConfigBase_GetEntryType)(void* _obj,wxString* name)
{
	return (int)((wxConfigBase*)_obj)->GetEntryType(*name);
}
	
EWXWEXPORT(wxString*,wxConfigBase_ReadString)(wxConfigBase* _obj,wxString* key,wxString* defVal)
{
	wxString tmp;
        tmp = ((wxConfigBase*)_obj)->Read(*key, *defVal);
	return new wxString(tmp);
}
	
EWXWEXPORT(int,wxConfigBase_ReadInteger)(void* _obj,wxString* key,int defVal)
{
	return ((wxConfigBase*)_obj)->Read(*key, defVal);
}
	
EWXWEXPORT(double,wxConfigBase_ReadDouble)(void* _obj,wxString* key,double defVal)
{
	double val;
	if (((wxConfigBase*)_obj)->Read(*key, &val, defVal))
 		return val;
 	return 0.0;
}
	
EWXWEXPORT(int,wxConfigBase_ReadBool)(wxConfigBase* _obj,wxString* key,int defVal)
{
	bool val;
	if (_obj->Read(*key, &val, defVal != 0))
		return val;
	return (int)false;
}
	
EWXWEXPORT(int,wxConfigBase_WriteString)(wxConfigBase* _obj,wxString* key,wxString* value)
{
	return (int)_obj->Write(*key, *value);
}

// FIXME: just left for backward-compatibiliry. wxHaskell uses int as long now.
EWXWEXPORT(int, wxConfigBase_WriteInteger)(void* _obj, void* key, int value)
{
	return (int)((wxConfigBase*)_obj)->Write((wxChar*)key, (long)value);
}
	
EWXWEXPORT(int,wxConfigBase_WriteLong)(wxConfigBase* _obj,wxString* key,long value)
{
	return (int)_obj->Write(*key, value);
}
	
EWXWEXPORT(int,wxConfigBase_WriteDouble)(wxConfigBase* _obj,wxString* key,double value)
{
	return (int)_obj->Write(*key, value);
}
	
EWXWEXPORT(int,wxConfigBase_WriteBool)(wxConfigBase* _obj,wxString* key,int value)
{
	return (int)_obj->Write(*key, value != 0);
}
	
EWXWEXPORT(int,wxConfigBase_Flush)(wxConfigBase* _obj,int bCurrentOnly)
{
	return (int)_obj->Flush(bCurrentOnly != 0);
}
	
EWXWEXPORT(int,wxConfigBase_RenameEntry)(wxConfigBase* _obj,wxString* oldName,wxString* newName)
{
	return (int)_obj->RenameEntry(*oldName, *newName);
}
	
EWXWEXPORT(int,wxConfigBase_RenameGroup)(wxConfigBase* _obj,wxString* oldName,wxString* newName)
{
	return (int)_obj->RenameGroup(*oldName, *newName);
}
	
EWXWEXPORT(int,wxConfigBase_DeleteEntry)(wxConfigBase* _obj,wxString* key,int bDeleteGroupIfEmpty)
{
	return (int)_obj->DeleteEntry(*key, bDeleteGroupIfEmpty != 0);
}
	
EWXWEXPORT(int,wxConfigBase_DeleteGroup)(wxConfigBase* _obj,wxString* key)
{
	return (int)_obj->DeleteGroup(*key);
}
	
EWXWEXPORT(int,wxConfigBase_DeleteAll)(wxConfigBase* _obj)
{
	return (int)_obj->DeleteAll();
}
	
EWXWEXPORT(int,wxConfigBase_IsExpandingEnvVars)(wxConfigBase* _obj)
{
	return (int)_obj->IsExpandingEnvVars();
}
	
EWXWEXPORT(void,wxConfigBase_SetExpandEnvVars)(wxConfigBase* _obj,int bDoIt)
{
	_obj->SetExpandEnvVars(bDoIt != 0);
}
	
EWXWEXPORT(void,wxConfigBase_SetRecordDefaults)(void* _obj,int bDoIt)
{
	((wxConfigBase*)_obj)->SetRecordDefaults(bDoIt != 0);
}
	
EWXWEXPORT(int,wxConfigBase_IsRecordingDefaults)(wxConfigBase* _obj)
{
	return (int)_obj->IsRecordingDefaults();
}
	
EWXWEXPORT(wxString*,wxConfigBase_ExpandEnvVars)(void* _obj,wxString* str)
{
	wxString *result = new wxString();
	*result = ((wxConfigBase*)_obj)->ExpandEnvVars(*str);
	return result;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetAppName)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxConfigBase*)_obj)->GetAppName();
	return result;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetVendorName)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxConfigBase*)_obj)->GetVendorName();
	return result;
}
	
EWXWEXPORT(void,wxConfigBase_SetAppName)(void* _obj,wxString* appName)
{
	((wxConfigBase*)_obj)->SetAppName(*appName);
}
	
EWXWEXPORT(void,wxConfigBase_SetVendorName)(void* _obj,wxString* vendorName)
{
	((wxConfigBase*)_obj)->SetVendorName(*vendorName);
}
	
EWXWEXPORT(void,wxConfigBase_SetStyle)(void* _obj,int style)
{
	((wxConfigBase*)_obj)->SetStyle((long)style);
}
	
EWXWEXPORT(int,wxConfigBase_GetStyle)(void* _obj)
{
	return (int)((wxConfigBase*)_obj)->GetStyle();
}
	
}
