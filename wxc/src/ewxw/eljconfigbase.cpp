#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxConfigBase_Create)()
{
	return (void*) wxConfigBase::Create();
}
	
EWXWEXPORT(void, wxConfigBase_Delete)(void* _obj)
{
	delete (wxConfigBase*)_obj;
}

EWXWEXPORT(void, wxConfigBase_SetPath)(void* _obj, void* strPath)
{
	((wxConfigBase*)_obj)->SetPath((char*) strPath);
}
	
EWXWEXPORT(int, wxConfigBase_GetPath)(void* _obj, void* _buf)
{
	wxString result = ((wxConfigBase*)_obj)->GetPath();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(int, wxConfigBase_GetFirstGroup)(void* _obj, void* str, void* lIndex, int len)
{
	wxString tmp;
	if (((wxConfigBase*)_obj)->GetFirstGroup(tmp, *((long*)lIndex)))
	{
		if ((unsigned int)len >= tmp.Length())
			memcpy(str, tmp.c_str(), tmp.Length());
		return tmp.Length();
	}
	else
		return 0;
}
	
EWXWEXPORT(int, wxConfigBase_GetNextGroup) (void* _obj, void* str, void* lIndex, int len)
{
	wxString tmp;
	if (((wxConfigBase*)_obj)->GetNextGroup(tmp, *((long*)lIndex)))
	{
		if ((unsigned int)len >= tmp.Length())
			memcpy(str, tmp.c_str(), tmp.Length());
		return tmp.Length();
	}
	else
		return 0;
}
	
EWXWEXPORT(int, wxConfigBase_GetFirstEntry)(void* _obj, void* str, void* lIndex, int len)
{
	wxString tmp;
	if (((wxConfigBase*)_obj)->GetFirstEntry(tmp, *((long*)lIndex)))
	{
		if ((unsigned int)len >= tmp.Length())
			memcpy(str, tmp.c_str(), tmp.Length());
		return tmp.Length();
	}
	else
		return 0;
}
	
EWXWEXPORT(int, wxConfigBase_GetNextEntry) (void* _obj, void* str, void* lIndex, int len)
{
	wxString tmp;
	if (((wxConfigBase*)_obj)->GetNextEntry(tmp, *((long*)lIndex)))
	{
		if ((unsigned int)len >= tmp.Length())
			memcpy(str, tmp.c_str(), tmp.Length());
		return tmp.Length();
	}
	else
		return 0;
}
	
EWXWEXPORT(int, wxConfigBase_GetNumberOfEntries)(void* _obj, int bRecursive)
{
	return (int)((wxConfigBase*)_obj)->GetNumberOfEntries(bRecursive != 0);
}
	
EWXWEXPORT(int, wxConfigBase_GetNumberOfGroups)(void* _obj, int bRecursive)
{
	return (int)((wxConfigBase*)_obj)->GetNumberOfGroups(bRecursive != 0);
}
	
EWXWEXPORT(int, wxConfigBase_HasGroup)(void* _obj, void* strName)
{
	return (int)((wxConfigBase*)_obj)->HasGroup((char*)strName);
}
	
EWXWEXPORT(int, wxConfigBase_HasEntry)(void* _obj, void* strName)
{
	return (int)((wxConfigBase*)_obj)->HasEntry((char*)strName);
}
	
EWXWEXPORT(int, wxConfigBase_Exists)(void* _obj, void* strName)
{
	return (int)((wxConfigBase*)_obj)->Exists((char*)strName);
}
	
EWXWEXPORT(int, wxConfigBase_GetEntryType)(void* _obj, void* name)
{
	return (int)((wxConfigBase*)_obj)->GetEntryType((char*)name);
}
	
EWXWEXPORT(int, wxConfigBase_ReadString)(void* _obj, void* key, void* pStr, void* defVal, int len)
{
	wxString tmp;
	if (((wxConfigBase*)_obj)->Read((char*)key, &tmp, (char*)defVal))
	{
		if ((unsigned int)len >= tmp.Length())
			memcpy(pStr, tmp.c_str(), tmp.Length());
		return tmp.Length();
	}
	else
		return 0;
}
	
EWXWEXPORT(int, wxConfigBase_ReadInteger)(void* _obj, void* key, int defVal)
{
	return ((wxConfigBase*)_obj)->Read((char*)key, defVal);
}
	
EWXWEXPORT(double, wxConfigBase_ReadDouble)(void* _obj, void* key, double defVal)
{
    double val;
	if (((wxConfigBase*)_obj)->Read((char*) key, &val, defVal))
 		return val;
 	return 0.0;
}
	
EWXWEXPORT(int, wxConfigBase_ReadBool)(void* _obj, void* key, int defVal)
{
	bool val;
	if (((wxConfigBase*)_obj)->Read((char*) key, &val, defVal != 0))
		return (int)val;
	return 0;
}
	
EWXWEXPORT(int, wxConfigBase_WriteString)(void* _obj, void* key, void* value)
{
	return (int)((wxConfigBase*)_obj)->Write((char*)key, (char*)value);
}
	
EWXWEXPORT(int, wxConfigBase_WriteInteger)(void* _obj, void* key, int value)
{
	return (int)((wxConfigBase*)_obj)->Write((char*)key, (long)value);
}
	
EWXWEXPORT(int, wxConfigBase_WriteDouble)(void* _obj, void* key, double value)
{
	return (int)((wxConfigBase*)_obj)->Write((char*)key, value);
}
	
EWXWEXPORT(int, wxConfigBase_WriteBool)(void* _obj, void* key, int value)
{
	return (int)((wxConfigBase*)_obj)->Write((char*)key, value != 0);
}
	
EWXWEXPORT(int, wxConfigBase_Flush)(void* _obj, int bCurrentOnly)
{
	return (int)((wxConfigBase*)_obj)->Flush(bCurrentOnly != 0);
}
	
EWXWEXPORT(int, wxConfigBase_RenameEntry)(void* _obj, void* oldName, void* newName)
{
	return (int)((wxConfigBase*)_obj)->RenameEntry((char*)oldName, (char*)newName);
}
	
EWXWEXPORT(int, wxConfigBase_RenameGroup)(void* _obj, void* oldName, void* newName)
{
	return (int)((wxConfigBase*)_obj)->RenameGroup((char*)oldName, (char*)newName);
}
	
EWXWEXPORT(int, wxConfigBase_DeleteEntry)(void* _obj, void* key, int bDeleteGroupIfEmpty)
{
	return (int)((wxConfigBase*)_obj)->DeleteEntry((char*)key, bDeleteGroupIfEmpty != 0);
}
	
EWXWEXPORT(int, wxConfigBase_DeleteGroup)(void* _obj, void* key)
{
	return (int)((wxConfigBase*)_obj)->DeleteGroup((char*)key);
}
	
EWXWEXPORT(int, wxConfigBase_DeleteAll)(void* _obj)
{
	return (int)((wxConfigBase*)_obj)->DeleteAll();
}
	
EWXWEXPORT(int, wxConfigBase_IsExpandingEnvVars)(void* _obj)
{
	return (int)((wxConfigBase*)_obj)->IsExpandingEnvVars();
}
	
EWXWEXPORT(void, wxConfigBase_SetExpandEnvVars)(void* _obj, int bDoIt)
{
	((wxConfigBase*)_obj)->SetExpandEnvVars(bDoIt != 0);
}
	
EWXWEXPORT(void, wxConfigBase_SetRecordDefaults)(void* _obj, int bDoIt)
{
	((wxConfigBase*)_obj)->SetRecordDefaults(bDoIt != 0);
}
	
EWXWEXPORT(int, wxConfigBase_IsRecordingDefaults)(void* _obj)
{
	return (int)((wxConfigBase*)_obj)->IsRecordingDefaults();
}
	
EWXWEXPORT(int, wxConfigBase_ExpandEnvVars)(void* _obj, void* str, void* _buf)
{
	wxString result = ((wxConfigBase*)_obj)->ExpandEnvVars((char*)str);
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(int, wxConfigBase_GetAppName)(void* _obj, void* _buf)
{
	wxString result = ((wxConfigBase*)_obj)->GetAppName();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(int, wxConfigBase_GetVendorName)(void* _obj, void* _buf)
{
	wxString result = ((wxConfigBase*)_obj)->GetVendorName();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(void, wxConfigBase_SetAppName)(void* _obj, void* appName)
{
	((wxConfigBase*)_obj)->SetAppName((char*)appName);
}
	
EWXWEXPORT(void, wxConfigBase_SetVendorName)(void* _obj, void* vendorName)
{
	((wxConfigBase*)_obj)->SetVendorName((char*)vendorName);
}
	
EWXWEXPORT(void, wxConfigBase_SetStyle)(void* _obj, int style)
{
	((wxConfigBase*)_obj)->SetStyle((long)style);
}
	
EWXWEXPORT(int, wxConfigBase_GetStyle)(void* _obj)
{
	return (int)((wxConfigBase*)_obj)->GetStyle();
}
	
}
