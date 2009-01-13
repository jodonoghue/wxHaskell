#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxDataFormat_CreateFromId)(wxString* name)
{
	return (void*) new wxDataFormat (*name);
}

EWXWEXPORT(void*,wxDataFormat_CreateFromType)(int typ)
{
	return (void*) new wxDataFormat ((wxDataFormat::NativeFormat)typ);
}

EWXWEXPORT(void,wxDataFormat_Delete)(void* _obj)
{
	delete (wxDataFormat*)_obj;
}

EWXWEXPORT(bool,wxDataFormat_IsEqual)(wxDataFormat* _obj,wxDataFormat* other)
{
	return  *_obj == *other;
}

EWXWEXPORT(wxString*,wxDataFormat_GetId)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxDataFormat*)_obj)->GetId();
	return result;
}

EWXWEXPORT(int,wxDataFormat_GetType)(void* _obj)
{
	return (int)((wxDataFormat*)_obj)->GetType();
}

EWXWEXPORT(void,wxDataFormat_SetId)(void* _obj,wxString* id)
{
	((wxDataFormat*)_obj)->SetId(* id);
}

EWXWEXPORT(void,wxDataFormat_SetType)(void* _obj,int typ)
{
#ifdef __WIN32__
	((wxDataFormat*)_obj)->SetType((wxDataFormat::NativeFormat)typ);
#else
	((wxDataFormat*)_obj)->SetType((wxDataFormatId)typ);
#endif
}

}
