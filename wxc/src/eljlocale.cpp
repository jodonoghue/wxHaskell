#include "wrapper.h"
#include "wx/intl.h"

extern "C"
{

EWXWEXPORT(void*,wxLocale_Create)(int _lang,int _flags)
{
	return (void*)new wxLocale(_lang, _flags);
}

EWXWEXPORT(void,wxLocale_Delete)(void* _obj)
{
	delete (wxLocale*)_obj;
}

EWXWEXPORT(bool,wxLocale_IsOk)(wxLocale* _obj)
{
	return _obj->IsOk();
}
	
EWXWEXPORT(void*,wxLocale_GetLocale)(void* _obj)
{
	return (void*)((wxLocale*)_obj)->GetLocale();
}
	
EWXWEXPORT(void,wxLocale_AddCatalogLookupPathPrefix)(void* _obj,void* prefix)
{
	((wxLocale*)_obj)->AddCatalogLookupPathPrefix((const wxChar*)prefix);
}
	
EWXWEXPORT(bool,wxLocale_AddCatalog)(wxLocale* _obj,void* szDomain)
{
	return _obj->AddCatalog((const wxChar*)szDomain);
}
	
EWXWEXPORT(bool,wxLocale_IsLoaded)(wxLocale* _obj,void* szDomain)
{
	return _obj->IsLoaded((const wxChar*)szDomain);
}
	
EWXWEXPORT(void*,wxLocale_GetString)(void* _obj,void* szOrigString,void* szDomain)
{
	return (void*)((wxLocale*)_obj)->GetString((const wxChar*)szOrigString, (const wxChar*)szDomain);
}
	
EWXWEXPORT(wxString*,wxLocale_GetName)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxLocale*)_obj)->GetName();
	return result;
}
	

EWXWEXPORT(void*,wxGetELJLocale)()
{
	return (void*)wxGetLocale();
}
	
EWXWEXPORT(void*,wxGetELJTranslation)(void* sz)
{
	return (void*)wxGetTranslation((const wxChar*)sz);
}
	
}
