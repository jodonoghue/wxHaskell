#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxCriticalSection_Create) ()
{
	return (void*) new wxCriticalSection ();
}

EWXWEXPORT(void, wxCriticalSection_Delete) (void* _obj)
{
	delete (wxCriticalSection*)_obj;
}

EWXWEXPORT(void, wxCriticalSection_Enter) (void* _obj)
{
	((wxCriticalSection*)_obj)->Enter();
}

EWXWEXPORT(void, wxCriticalSection_Leave) (void* _obj)
{
	((wxCriticalSection*)_obj)->Leave();
}


EWXWEXPORT(void*, wxMutex_Create) ()
{
	return (void*) new wxMutex ();
}

EWXWEXPORT(void, wxMutex_Delete) (void* _obj)
{
	delete (wxMutex*)_obj;
}

EWXWEXPORT(int, wxMutex_Lock) (void* _obj)
{
	return (int)((wxMutex*)_obj)->Lock();
}

EWXWEXPORT(int, wxMutex_TryLock) (void* _obj)
{
	return (int)((wxMutex*)_obj)->TryLock();
}

EWXWEXPORT(int, wxMutex_Unlock) (void* _obj)
{
	return (int)((wxMutex*)_obj)->Unlock();
}

EWXWEXPORT(int, wxMutex_IsLocked) (void* _obj)
{
#if wxVERSION_NUMBER >= 2400
	return 0;
#else
	return (int)((wxMutex*)_obj)->IsLocked();
#endif
}

EWXWEXPORT(void*, wxCondition_Create) (void* _mutex)
{
#if wxVERSION_NUMBER < 2400
	return (void*) new wxCondition ();
#else
	return (void*) new wxCondition (*((wxMutex*)_mutex));
#endif
}

EWXWEXPORT(void, wxCondition_Delete) (void* _obj)
{
	delete (wxCondition*)_obj;
}

EWXWEXPORT(void, wxCondition_Wait) (void* _obj)
{
	((wxCondition*)_obj)->Wait();
}

EWXWEXPORT(int, wxCondition_WaitFor) (void* _obj, int sec, int nsec)
{
#if wxVERSION_NUMBER >= 2400
	return (int)((wxCondition*)_obj)->WaitTimeout((unsigned long)nsec);
#else
	return (int)((wxCondition*)_obj)->Wait((unsigned long)sec, (unsigned long)nsec);
#endif
}

EWXWEXPORT(void, wxCondition_Signal) (void* _obj)
{
	((wxCondition*)_obj)->Signal();
}

EWXWEXPORT(void, wxCondition_Broadcast) (void* _obj)
{
	((wxCondition*)_obj)->Broadcast();
}


EWXWEXPORT(void, wxMutexGui_Enter) ()
{
	wxMutexGuiEnter();
}

EWXWEXPORT(void, wxMutexGui_Leave) ()
{
	wxMutexGuiLeave();
}

}
