#include "wrapper.h"
#include "wx/mimetype.h"

extern "C"
{

EWXWEXPORT(void*,wxMimeTypesManager_Create)()
{
        return (void*) wxTheMimeTypesManager;
}

EWXWEXPORT(void*,wxMimeTypesManager_GetFileTypeFromExtension)(void* _obj,wxString* _ext)
{
        return (void*)((wxMimeTypesManager*)_obj)->GetFileTypeFromExtension(*_ext);
}

EWXWEXPORT(void*,wxMimeTypesManager_GetFileTypeFromMimeType)(void* _obj,wxString* _name)
{
        return (void*)((wxMimeTypesManager*)_obj)->GetFileTypeFromMimeType(*_name);
}

EWXWEXPORT(int,wxMimeTypesManager_ReadMailcap)(wxMimeTypesManager* _obj,wxString* _file,int _fb)
{
        return (int)_obj->ReadMailcap(*_file, _fb != 0);
}\

EWXWEXPORT(int,wxMimeTypesManager_ReadMimeTypes)(wxMimeTypesManager* _obj,wxString* _file)
{
        return (int)_obj->ReadMimeTypes(*_file);
}

EWXWEXPORT(int,wxMimeTypesManager_EnumAllFileTypes)(wxMimeTypesManager* _obj,void* _lst)
{
        wxArrayString arr;
        int result = (int)_obj->EnumAllFileTypes(arr);

        if (_lst)
        {
                for (unsigned int i = 0; i < arr.GetCount(); i++)
                        ((const wxChar**)_lst)[i] = wxStrdup (arr.Item(i).c_str());
        }

        return result;
}

EWXWEXPORT(void,wxMimeTypesManager_AddFallbacks)(void* _obj,void* _types)
{
        ((wxMimeTypesManager*)_obj)->AddFallbacks((const wxFileTypeInfo*)_types);
}

EWXWEXPORT(int,wxMimeTypesManager_IsOfType)(wxMimeTypesManager* _obj,wxString* _type,wxString* _wildcard)
{
        return (int)_obj->IsOfType (*_type, *_wildcard);
}


EWXWEXPORT(wxString*,wxFileType_GetMimeType)(void* _obj)
{
        wxString *result = new wxString();
        if ((((wxFileType*)_obj)->GetMimeType(result))!=true)
          result->Clear();
        return result;
}

EWXWEXPORT(int,wxFileType_GetMimeTypes)(void* _obj,void* _lst)
{
        wxArrayString arr;

        if (((wxFileType*)_obj)->GetMimeTypes(arr) && _lst)
        {
                for (unsigned int i = 0; i < arr.GetCount(); i++)
                        ((const wxChar**)_lst)[i] = wxStrdup (arr.Item(i).c_str());
        }

        return arr.GetCount();
}

EWXWEXPORT(int,wxFileType_GetExtensions)(void* _obj,void* _lst)
{
        wxArrayString arr;

        if (((wxFileType*)_obj)->GetExtensions(arr) && _lst)
        {
                for (unsigned int i = 0; i < arr.GetCount(); i++)
                        ((const wxChar**)_lst)[i] = wxStrdup (arr.Item(i).c_str());
        }

        return arr.GetCount();
}

EWXWEXPORT(int,wxFileType_GetIcon)(wxFileType* _obj,wxIcon* icon)
{
#if wxCHECK_VERSION(2,5,0)
	wxIconLocation iconLoc;
	bool res = _obj->GetIcon(&iconLoc);
	*icon = wxIcon(iconLoc);
	return (int)res;
#else
	return (int)_obj->GetIcon(icon);
#endif
}

EWXWEXPORT(wxString*,wxFileType_GetDescription)(void* _obj)
{
        wxString *result = new wxString();
        if (((wxFileType*)_obj)->GetDescription(result) != true)
          result->Clear();
        return result;
}

EWXWEXPORT(wxString*,wxFileType_GetOpenCommand)(void* _obj,void* _params)
{
        wxString *result = new wxString();
        if (((wxFileType*)_obj)->GetOpenCommand(result, *((wxFileType::MessageParameters*)_params)) != true)
          result->Clear();
        return result;
}

EWXWEXPORT(wxString*,wxFileType_GetPrintCommand)(void* _obj,void* _params)
{
        wxString *result = new wxString();
        if (((wxFileType*)_obj)->GetPrintCommand(result, *((wxFileType::MessageParameters*)_params)) != true)
          result->Clear();
        return result;
}

EWXWEXPORT(wxString*,wxFileType_ExpandCommand)(void* _obj,void* _cmd,void* _params)
{
        wxString *result = new wxString();
        *result = ((wxFileType*)_obj)->ExpandCommand((const wxChar*)_cmd, *((wxFileType::MessageParameters*)_params));
        return result;
}

EWXWEXPORT(void,wxFileType_Delete)(void* _obj)
{
        delete (wxFileType*)_obj;
}


}
