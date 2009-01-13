#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,ELJConnection_CreateDefault)()
{
	return new ELJConnection();
}

EWXWEXPORT(void*,ELJConnection_Create)(wxChar* buffer,int size)
{
	return new ELJConnection(buffer, size);
}

EWXWEXPORT(void,ELJConnection_Delete)(void* _obj)
{
	delete (ELJConnection*)_obj;
}

EWXWEXPORT(int,ELJConnection_Execute)(ELJConnection* _obj,wxString* data,int size,int format)
{
	return (int)_obj->Execute(*data, size, (wxIPCFormat)format);
}
	
EWXWEXPORT(void*,ELJConnection_Request)(void* _obj,wxString* item,void* size,int format)
{
	return (void*)((ELJConnection*)_obj)->Request(*item, (int*)size, (wxIPCFormat)format);
}
	
EWXWEXPORT(int,ELJConnection_Poke)(ELJConnection* _obj,wxString* item,wxChar* data,int size,int format)
{
	return (int)_obj->Poke(*item, data, size, (wxIPCFormat)format);
}
	
EWXWEXPORT(int,ELJConnection_StartAdvise)(ELJConnection* _obj,wxString* item)
{
	return (int)_obj->StartAdvise(*item);
}
	
EWXWEXPORT(int,ELJConnection_StopAdvise)(ELJConnection* _obj,wxString* item)
{
	return (int)_obj->StopAdvise(*item);
}
	
EWXWEXPORT(int,ELJConnection_Advise)(ELJConnection* _obj,wxString* item,wxChar* data,int size,int format)
{
	return (int)_obj->Advise(*item, data, size, (wxIPCFormat)format);
}
	
EWXWEXPORT(int,ELJConnection_Disconnect)(ELJConnection* _obj)
{
	return (int)_obj->Disconnect();
}
	
EWXWEXPORT(void,ELJConnection_Compress)(void* _obj,int on)
{
	((ELJConnection*)_obj)->Compress(on != 0);
}
	
EWXWEXPORT(void,ELJConnection_SetOnAdvise)(void* _obj,void* _fnc)
{
	((ELJConnection*)_obj)->SetOnAdvise(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnExecute)(void* _obj,void* _fnc)
{
	((ELJConnection*)_obj)->SetOnExecute(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnRequest)(void* _obj,void* _fnc)
{
	((ELJConnection*)_obj)->SetOnRequest(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnPoke)(void* _obj,void* _fnc)
{
	((ELJConnection*)_obj)->SetOnPoke(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnStartAdvise)(void* _obj,void* _fnc)
{
	((ELJConnection*)_obj)->SetOnStartAdvise(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnStopAdvise)(void* _obj,void* _fnc)
{
	((ELJConnection*)_obj)->SetOnStopAdvise(_fnc);
}

EWXWEXPORT(void,ELJConnection_SetOnDisconnect)(void* _obj,void* _fnc)
{
	((ELJConnection*)_obj)->SetOnDisconnect(_fnc);
}

EWXWEXPORT(void*,ELJServer_Create)(void* _eobj,void* _cnct)
{
	return new ELJServer(_eobj, _cnct);
}

EWXWEXPORT(void,ELJServer_Delete)(void* _obj)
{
	delete (ELJServer*)_obj;
}

EWXWEXPORT(int,ELJServer_Initialize)(void* _obj,wxString* name)
{
	return ((ELJServer*)_obj)->Create(*name);
}
	
EWXWEXPORT(void*,ELJClient_Create)(void* _eobj,void* _cnct)
{
	return new ELJClient(_eobj, _cnct);
}

EWXWEXPORT(void,ELJClient_Delete)(void* _obj)
{
	delete (ELJClient*)_obj;
}

EWXWEXPORT(void,ELJClient_MakeConnection)(void* _obj,wxString* host,wxString* server,wxString* topic)
{
	((ELJClient*)_obj)->MakeConnection(*host, *server, *topic);
}
	
}
