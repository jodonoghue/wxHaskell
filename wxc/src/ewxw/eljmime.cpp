#include "wrapper.h"
#include "wx/mimetype.h"

extern "C"
{

EWXWEXPORT(void*, wxMimeTypesManager_Create) ()
{
	return (void*) wxTheMimeTypesManager;
}

EWXWEXPORT(void*, wxMimeTypesManager_GetFileTypeFromExtension) (void* _obj, void* _ext)
{
	return (void*)((wxMimeTypesManager*)_obj)->GetFileTypeFromExtension((const char*)_ext);
}
	
EWXWEXPORT(void*, wxMimeTypesManager_GetFileTypeFromMimeType) (void* _obj, void* _name)
{
	return (void*)((wxMimeTypesManager*)_obj)->GetFileTypeFromMimeType((const char*)_name);
}
	
EWXWEXPORT(int, wxMimeTypesManager_ReadMailcap) (void* _obj, void* _file, int _fb)
{
	return (int)((wxMimeTypesManager*)_obj)->ReadMailcap((const char*)_file, _fb != 0);
}
	
EWXWEXPORT(int, wxMimeTypesManager_ReadMimeTypes) (void* _obj, void* _file)
{
	return (int)((wxMimeTypesManager*)_obj)->ReadMimeTypes((const char*)_file);
}
	
EWXWEXPORT(int, wxMimeTypesManager_EnumAllFileTypes) (void* _obj, void* _lst)
{
	wxArrayString arr;
	int result = (int)((wxMimeTypesManager*)_obj)->EnumAllFileTypes(arr);
	
	if (_lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((const char**)_lst)[i] = strdup (arr.Item(i).c_str());
	}
	
	return result;
}
	
EWXWEXPORT(void, wxMimeTypesManager_AddFallbacks) (void* _obj, void* _types)
{
	((wxMimeTypesManager*)_obj)->AddFallbacks((const wxFileTypeInfo*)_types);
}

EWXWEXPORT(int, wxMimeTypesManager_IsOfType)(void* _obj, void* _type, void* _wildcard)
{
	return (int)((wxMimeTypesManager*)_obj)->IsOfType ((const char*)_type, (const char*)_wildcard);
}
	

EWXWEXPORT(int, wxFileType_GetMimeType) (void* _obj, void* _buf)
{
	wxString result;
	
	if (((wxFileType*)_obj)->GetMimeType(&result) && _buf)
		memcpy (_buf, result.c_str(), result.Length());
	
	return result.Length();
}
	
EWXWEXPORT(int, wxFileType_GetMimeTypes) (void* _obj, void* _lst)
{
	wxArrayString arr;

	if (((wxFileType*)_obj)->GetMimeTypes(arr) && _lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((const char**)_lst)[i] = strdup (arr.Item(i).c_str());
	}
	
	return arr.GetCount();
}
	
EWXWEXPORT(int, wxFileType_GetExtensions) (void* _obj, void* _lst)
{
	wxArrayString arr;

	if (((wxFileType*)_obj)->GetExtensions(arr) && _lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((const char**)_lst)[i] = strdup (arr.Item(i).c_str());
	}
	
	return arr.GetCount();
}
	
EWXWEXPORT(int, wxFileType_GetIcon) (void* _obj, void *icon)
{
	return (int)((wxFileType*)_obj)->GetIcon((wxIcon*)icon);
}
	
EWXWEXPORT(int, wxFileType_GetDescription) (void* _obj, void* _buf)
{
	wxString result;
	
	if (((wxFileType*)_obj)->GetDescription(&result) && _buf)
		memcpy (_buf, result.c_str(), result.Length());
	
	return result.Length();
}
	
EWXWEXPORT(int, wxFileType_GetOpenCommand) (void* _obj, void* _buf, void* _params)
{
	wxString result;
	
	if (((wxFileType*)_obj)->GetOpenCommand(&result, *((wxFileType::MessageParameters*)_params)) && _buf)
		memcpy (_buf, result.c_str(), result.Length());

	return result.Length();
}
	
EWXWEXPORT(int, wxFileType_GetPrintCommand) (void* _obj, void* _buf, void* _params)
{
	wxString result;
	
	if (((wxFileType*)_obj)->GetPrintCommand(&result, *((wxFileType::MessageParameters*)_params)) && _buf)
		memcpy (_buf, result.c_str(), result.Length());

	return result.Length();
}
	
EWXWEXPORT(int, wxFileType_ExpandCommand) (void* _obj, void* _cmd, void* _params, void* _buf)
{
	wxString result = ((wxFileType*)_obj)->ExpandCommand((const char*)_cmd, *((wxFileType::MessageParameters*)_params));
	
	if (_buf)
		memcpy (_buf, result.c_str(), result.Length());

	return result.Length();
}
	
EWXWEXPORT(void, wxFileType_Delete) (void* _obj)
{
	delete (wxFileType*)_obj;
}


class ELJMessageParameters : public wxFileType::MessageParameters
{
	private:
		void* EiffelObject;
		TGetText EiffelFunc;
	public:
		ELJMessageParameters(const wxString& filename, const wxString& mimetype, void* _object, void* _func)
		: wxFileType::MessageParameters(filename, mimetype)
	{
		EiffelFunc = (TGetText)_func;
		EiffelObject = _object;
	}

	virtual wxString GetParamValue(const wxString& name) const
	{
		if (EiffelObject)
		{
			void* res = EiffelFunc(EiffelObject, (void*)name.c_str());
			if (res)
				return (const char*)res;
			else
				return wxFileType::MessageParameters::GetParamValue(name);
		}
		else
			return wxFileType::MessageParameters::GetParamValue(name);
   }
};

EWXWEXPORT(void*, wxMessageParameters_Create) (void* _file, void* _type, void* _object, void* _func)
{
	return (void*)new ELJMessageParameters((const char*)_file, (const char*)_type, _object, _func);
}

EWXWEXPORT(void, wxMessageParameters_Delete) (void* _obj)
{
	delete (ELJMessageParameters*)_obj;
}

}
