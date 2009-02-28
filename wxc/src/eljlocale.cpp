#include "wrapper.h"
#include "wx/intl.h"

extern "C"
{

EWXWEXPORT(void*,wxLocale_Create)(int _lang,int _flags)
{
	return (void*)new wxLocale(_lang, _flags);
}

EWXWEXPORT(void,wxLocale_Delete)(wxLocale* self)
{
	delete self;
}

EWXWEXPORT(bool,wxLocale_IsOk)(wxLocale* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(void*,wxLocale_GetLocale)(wxLocale* self)
{
	return (void*)self->GetLocale();
}
	
EWXWEXPORT(void,wxLocale_AddCatalogLookupPathPrefix)(wxLocale* self,void* prefix)
{
	self->AddCatalogLookupPathPrefix((const wxChar*)prefix);
}
	
EWXWEXPORT(bool,wxLocale_AddCatalog)(wxLocale* self,void* szDomain)
{
	return self->AddCatalog((const wxChar*)szDomain);
}
	
EWXWEXPORT(bool,wxLocale_IsLoaded)(wxLocale* self,void* szDomain)
{
	return self->IsLoaded((const wxChar*)szDomain);
}
	
EWXWEXPORT(void*,wxLocale_GetString)(wxLocale* self,void* szOrigString,void* szDomain)
{
	return (void*)self->GetString((const wxChar*)szOrigString, (const wxChar*)szDomain);
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
