#include "wrapper.h"
#include "wx/intl.h"

extern "C"
{

EWXWEXPORT(void*,wxLocale_Create)(int _lang, int _flags)
{
	return (void*)new wxLocale(_lang, _flags);
}

EWXWEXPORT(void,wxLocale_Delete)(void* _obj)
{
	delete (wxLocale*)_obj;
}

EWXWEXPORT(int,wxLocale_IsOk)(void* _obj)
{
	return (int)((wxLocale*)_obj)->IsOk();
}
	
EWXWEXPORT(void*,wxLocale_GetLocale)(void* _obj)
{
	return (void*)((wxLocale*)_obj)->GetLocale();
}
	
EWXWEXPORT(void,wxLocale_AddCatalogLookupPathPrefix)(void* _obj, void* prefix)
{
	((wxLocale*)_obj)->AddCatalogLookupPathPrefix((const char*)prefix);
}
	
EWXWEXPORT(int,wxLocale_AddCatalog)(void* _obj, void* szDomain)
{
	return (int)((wxLocale*)_obj)->AddCatalog((const wxChar*)szDomain);
}
	
EWXWEXPORT(int,wxLocale_IsLoaded)(void* _obj, void* szDomain)
{
	return (int)((wxLocale*)_obj)->IsLoaded((const wxChar*)szDomain);
}
	
EWXWEXPORT(void*,wxLocale_GetString)(void* _obj, void* szOrigString, void* szDomain)
{
	return (void*)((wxLocale*)_obj)->GetString((const wxChar*)szOrigString, (const wxChar*)szDomain);
}
	
EWXWEXPORT(int,wxLocale_GetName)(void* _obj, void* _ref)
{
	wxString res = ((wxLocale*)_obj)->GetName();
	if (_ref) memcpy (_ref, res.c_str(), res.Length());
	return res.Length();
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
