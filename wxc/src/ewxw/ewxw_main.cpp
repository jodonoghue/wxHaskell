#include "wrapper.h"

extern int APPTerminating;

#ifdef __WIN32__
#include <windows.h>

#if wxVERSION_NUMBER >= 2400
extern HINSTANCE wxhInstance;
#else
HINSTANCE wxhInstance = 0;
#endif

extern "C"
{

EWXWEXPORT(void, ELJApp_initialize) (void* _obj, AppInitFunc _func, void* _cmd, void* _inst)
{
	wxhInstance = (HINSTANCE) _inst;
	init_func = _func;
	eiffel_obj = _obj;
	wxEntry(_inst, NULL, (char*)_cmd, SW_SHOWNORMAL, true);
	APPTerminating = 1;
}

EWXWEXPORT(void, ELJModule_initialize) (void* _inst, void* _app)
{
	wxhInstance = (HINSTANCE) _inst;
    wxApp::Initialize();

	wxTheApp = (wxApp*)_app;

    wxTheApp->argc = 0;
    wxTheApp->argv = NULL;
}

BOOL WINAPI DllMain (HANDLE hModule, DWORD fdwReason, LPVOID lpReserved)
{
//	wxhInstance = (HINSTANCE)hModule;
	return TRUE;
}

#if wxVERSION_NUMBER < 2400
HINSTANCE wxGetInstance()
{
	return wxhInstance;
}

void wxSetInstance(HINSTANCE hInst)
{
	wxhInstance = hInst;
}
#endif

}

#else
extern "C"
{

EWXWEXPORT(void, ELJApp_initialize) (void* _obj, AppInitFunc _func, int _argc, void* _argv)
{
	init_func = _func;
	eiffel_obj = _obj;
	wxEntry(_argc, (char**)_argv);
	APPTerminating = 1;
}

}
#endif
