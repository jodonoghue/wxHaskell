#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxDataFormat_CreateFromId) (void* name)
{
	return (void*) new wxDataFormat ((char*)name);
}

EWXWEXPORT(void*, wxDataFormat_CreateFromType) (int typ)
{
	return (void*) new wxDataFormat ((wxDataFormat::NativeFormat)typ);
}

EWXWEXPORT(void, wxDataFormat_Delete) (void* _obj)
{
	delete (wxDataFormat*)_obj;
}

EWXWEXPORT(int, wxDataFormat_IsEqual) (void* _obj, void* other)
{
	return (int) (*(wxDataFormat*)_obj == *(wxDataFormat*)other);
}

EWXWEXPORT(int, wxDataFormat_GetId) (void* _obj, void* _buf)
{
	wxString result =((wxDataFormat*)_obj)->GetId();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}

EWXWEXPORT(int, wxDataFormat_GetType) (void* _obj)
{
	return (int)((wxDataFormat*)_obj)->GetType();
}

EWXWEXPORT(void, wxDataFormat_SetId) (void* _obj, void* id)
{
	((wxDataFormat*)_obj)->SetId((char*) id);
}

EWXWEXPORT(void, wxDataFormat_SetType) (void* _obj, int typ)
{
#ifdef __WIN32__
	((wxDataFormat*)_obj)->SetType((wxDataFormat::NativeFormat)typ);
#else
	((wxDataFormat*)_obj)->SetType((wxDataFormatId)typ);
#endif
}

}
