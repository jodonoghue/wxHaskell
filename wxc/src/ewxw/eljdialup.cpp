#include "wrapper.h"
#include "wx/dialup.h"

extern "C"
{

EWXWEXPORT(void*,wxDialUpManager_Create)()
{
	return (void*) wxDialUpManager::Create();
}

EWXWEXPORT(void,wxDialUpManager_Delete)(void* _obj)
{
	delete (wxDialUpManager*)_obj;
}

EWXWEXPORT(int,wxDialUpManager_IsOk)(void* _obj)
{
	return (int)((wxDialUpManager*)_obj)->IsOk();
}
	
EWXWEXPORT(int,wxDialUpManager_GetISPNames)(void* _obj, void* _lst)
{
	wxArrayString arr;
	((wxDialUpManager*)_obj)->GetISPNames(arr);

	if (_lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
#ifdef wxUSE_UNICODE
			((wxChar**)_lst)[i] = wcsdup(arr.Item(i).c_str());
#else
			((wxChar**)_lst)[i] = strdup(arr.Item(i).c_str());
#endif
	}

	return arr.GetCount();
}
	
EWXWEXPORT(int,wxDialUpManager_Dial)(void* _obj, void* nameOfISP, void* username, void* password, int async)
{
	return (int)((wxDialUpManager*)_obj)->Dial((const wxChar*)nameOfISP, (const wxChar*)username, (const wxChar*)password, async != 0);
}
	
EWXWEXPORT(int,wxDialUpManager_IsDialing)(void* _obj)
{
	return (int)((wxDialUpManager*)_obj)->IsDialing();
}
	
EWXWEXPORT(int,wxDialUpManager_CancelDialing)(void* _obj)
{
	return (int)((wxDialUpManager*)_obj)->CancelDialing();
}
	
EWXWEXPORT(int,wxDialUpManager_HangUp)(void* _obj)
{
	return (int)((wxDialUpManager*)_obj)->HangUp();
}
	
EWXWEXPORT(int,wxDialUpManager_IsAlwaysOnline)(void* _obj)
{
	return (int)((wxDialUpManager*)_obj)->IsAlwaysOnline();
}
	
EWXWEXPORT(int,wxDialUpManager_IsOnline)(void* _obj)
{
	return (int)((wxDialUpManager*)_obj)->IsOnline();
}
	
EWXWEXPORT(void,wxDialUpManager_SetOnlineStatus)(void* _obj, int isOnline)
{
	((wxDialUpManager*)_obj)->SetOnlineStatus(isOnline != 0);
}
	
EWXWEXPORT(int,wxDialUpManager_EnableAutoCheckOnlineStatus)(void* _obj, int nSeconds)
{
	return (int)((wxDialUpManager*)_obj)->EnableAutoCheckOnlineStatus((size_t)nSeconds);
}
	
EWXWEXPORT(void,wxDialUpManager_DisableAutoCheckOnlineStatus)(void* _obj)
{
	((wxDialUpManager*)_obj)->DisableAutoCheckOnlineStatus();
}
	
EWXWEXPORT(void,wxDialUpManager_SetWellKnownHost)(void* _obj, void* hostname, int portno)
{
	((wxDialUpManager*)_obj)->SetWellKnownHost((const wxChar*)hostname, portno);
}
	
EWXWEXPORT(void,wxDialUpManager_SetConnectCommand)(void* _obj, void* commandDial, void* commandHangup)
{
	((wxDialUpManager*)_obj)->SetConnectCommand((const wxChar*)commandDial, (const wxChar*)commandHangup);
}
	

EWXWEXPORT(int,wxDialUpEvent_IsConnectedEvent)(void* _obj)
{
	return (int)((wxDialUpEvent*)_obj)->IsConnectedEvent();
}
	
EWXWEXPORT(int,wxDialUpEvent_IsOwnEvent)(void* _obj)
{
	return (int)((wxDialUpEvent*)_obj)->IsOwnEvent();
}
	
}
