#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/snglinst.h"

extern "C"
{

EWXWEXPORT(void*,wxSingleInstanceChecker_CreateDefault)()
{
	return (void*)new wxSingleInstanceChecker();
}
	
EWXWEXPORT(bool,wxSingleInstanceChecker_Create)(wxSingleInstanceChecker* _obj,wxString* name,wxString* path)
{
	return _obj->Create(*name, *path);
}
	
EWXWEXPORT(bool,wxSingleInstanceChecker_IsAnotherRunning)(wxSingleInstanceChecker* _obj)
{
	return _obj->IsAnotherRunning();
}
	
EWXWEXPORT(void,wxSingleInstanceChecker_Delete)(void* _obj)
{
	delete (wxSingleInstanceChecker*)_obj;
}
	
}
#endif
