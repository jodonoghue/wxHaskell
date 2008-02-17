#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxTimer_Create) (void* _prt, int _id)
{
	return (void*) new wxTimer ((wxEvtHandler*)_prt, _id);
}

EWXWEXPORT(void, wxTimer_Delete) (void* _obj)
{
	delete (wxTimer*)_obj;
}

EWXWEXPORT(int, wxTimer_Start) (void* _obj, int _int, int _one)
{
	return (int)((wxTimer*)_obj)->Start (_int, _one != 0);
}

EWXWEXPORT(void, wxTimer_Stop) (void* _obj)
{
	((wxTimer*)_obj)->Stop();
}

EWXWEXPORT(int, wxTimer_IsRuning) (void* _obj)
{
	return (int)((wxTimer*)_obj)->IsRunning();
}

EWXWEXPORT(int, wxTimer_IsOneShot) (void* _obj)
{
	return (int)((wxTimer*)_obj)->IsOneShot();
}

EWXWEXPORT(int, wxTimer_GetInterval) (void* _obj)
{
	return ((wxTimer*)_obj)->GetInterval();
}


EWXWEXPORT(void*,wxStopWatch_Create)()
{
	return (void*)new wxStopWatch();
}
	
EWXWEXPORT(void,wxStopWatch_Delete)(void* _obj)
{
	delete (wxStopWatch*)_obj;
}
	
EWXWEXPORT(void,wxStopWatch_Start)(void* _obj, int _t)
{
	((wxStopWatch*)_obj)->Start((long)_t);
}
	
EWXWEXPORT(void,wxStopWatch_Pause)(void* _obj)
{
	((wxStopWatch*)_obj)->Pause();
}
	
EWXWEXPORT(void,wxStopWatch_Resume)(void* _obj)
{
	((wxStopWatch*)_obj)->Resume();
}
	
EWXWEXPORT(int,wxStopWatch_Time)(void* _obj)
{
	return (int)((wxStopWatch*)_obj)->Time();
}
	
}
