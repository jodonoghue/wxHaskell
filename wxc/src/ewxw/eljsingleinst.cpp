#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/snglinst.h"

extern "C"
{

EWXWEXPORT(void*,wxSingleInstanceChecker_CreateDefault)()
{
	return (void*)new wxSingleInstanceChecker();
}
	
EWXWEXPORT(int,wxSingleInstanceChecker_Create)(void* _obj, void* name, void* path)
{
	return (int)((wxSingleInstanceChecker*)_obj)->Create((wxChar*)name, (wxChar*)path);
}
	
EWXWEXPORT(int,wxSingleInstanceChecker_IsAnotherRunning)(void* _obj)
{
	return (int)((wxSingleInstanceChecker*)_obj)->IsAnotherRunning();
}
	
EWXWEXPORT(void,wxSingleInstanceChecker_Delete)(void* _obj)
{
	delete (wxSingleInstanceChecker*)_obj;
}
	
}
#endif
