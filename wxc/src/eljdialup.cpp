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

EWXWEXPORT(int,wxDialUpManager_IsOk)(wxDialUpManager* _obj)
{
	return (int)_obj->IsOk();
}
	
EWXWEXPORT(int,wxDialUpManager_GetISPNames)(void* _obj,void* _lst)
{
	wxArrayString arr;
	((wxDialUpManager*)_obj)->GetISPNames(arr);

	if (_lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((wxChar**)_lst)[i] = wxStrdup(arr.Item(i).c_str());
	}

	return arr.GetCount();
}
	
EWXWEXPORT(int,wxDialUpManager_Dial)(wxDialUpManager* _obj,wxString* nameOfISP,wxString* username,wxString* password,int async)
{
	return (int)_obj->Dial(*nameOfISP, *username, *password, async != 0);
}
	
EWXWEXPORT(int,wxDialUpManager_IsDialing)(wxDialUpManager* _obj)
{
	return (int)_obj->IsDialing();
}
	
EWXWEXPORT(int,wxDialUpManager_CancelDialing)(wxDialUpManager* _obj)
{
	return (int)_obj->CancelDialing();
}
	
EWXWEXPORT(int,wxDialUpManager_HangUp)(wxDialUpManager* _obj)
{
	return (int)_obj->HangUp();
}
	
EWXWEXPORT(int,wxDialUpManager_IsAlwaysOnline)(wxDialUpManager* _obj)
{
	return (int)_obj->IsAlwaysOnline();
}
	
EWXWEXPORT(int,wxDialUpManager_IsOnline)(wxDialUpManager* _obj)
{
	return (int)_obj->IsOnline();
}
	
EWXWEXPORT(void,wxDialUpManager_SetOnlineStatus)(void* _obj,int isOnline)
{
	((wxDialUpManager*)_obj)->SetOnlineStatus(isOnline != 0);
}
	
EWXWEXPORT(int,wxDialUpManager_EnableAutoCheckOnlineStatus)(wxDialUpManager* _obj,int nSeconds)
{
	return (int)_obj->EnableAutoCheckOnlineStatus((size_t)nSeconds);
}
	
EWXWEXPORT(void,wxDialUpManager_DisableAutoCheckOnlineStatus)(void* _obj)
{
	((wxDialUpManager*)_obj)->DisableAutoCheckOnlineStatus();
}
	
EWXWEXPORT(void,wxDialUpManager_SetWellKnownHost)(void* _obj,void* hostname,int portno)
{
	((wxDialUpManager*)_obj)->SetWellKnownHost((const wxChar*)hostname, portno);
}
	
EWXWEXPORT(void,wxDialUpManager_SetConnectCommand)(void* _obj,void* commandDial,void* commandHangup)
{
	((wxDialUpManager*)_obj)->SetConnectCommand((const wxChar*)commandDial, (const wxChar*)commandHangup);
}
	

EWXWEXPORT(int,wxDialUpEvent_IsConnectedEvent)(wxDialUpEvent* _obj)
{
	return (int)_obj->IsConnectedEvent();
}
	
EWXWEXPORT(int,wxDialUpEvent_IsOwnEvent)(wxDialUpEvent* _obj)
{
	return (int)_obj->IsOwnEvent();
}
	
}
