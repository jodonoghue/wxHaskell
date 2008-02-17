#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, ELJConnection_CreateDefault)()
{
	return new ELJConnection();
}

EWXWEXPORT(void*, ELJConnection_Create)(void* buffer, int size)
{
	return new ELJConnection((wxChar*)buffer, size);
}

EWXWEXPORT(void, ELJConnection_Delete)(void* _obj)
{
	delete (ELJConnection*)_obj;
}

EWXWEXPORT(int, ELJConnection_Execute)(void* _obj, void* data, int size, int format)
{
	return (int)((ELJConnection*)_obj)->Execute((wxChar*)data, size, (wxIPCFormat)format);
}
	
EWXWEXPORT(void*, ELJConnection_Request)(void* _obj, void* item, void* size, int format)
{
	return (void*)((ELJConnection*)_obj)->Request((wxChar*)item, (int*)size, (wxIPCFormat)format);
}
	
EWXWEXPORT(int, ELJConnection_Poke)(void* _obj, void* item, void* data, int size, int format)
{
	return (int)((ELJConnection*)_obj)->Poke((wxChar*)item, (wxChar*)data, size, (wxIPCFormat)format);
}
	
EWXWEXPORT(int, ELJConnection_StartAdvise)(void* _obj, void* item)
{
	return (int)((ELJConnection*)_obj)->StartAdvise((wxChar*)item);
}
	
EWXWEXPORT(int, ELJConnection_StopAdvise)(void* _obj, void* item)
{
	return (int)((ELJConnection*)_obj)->StopAdvise((wxChar*)item);
}
	
EWXWEXPORT(int, ELJConnection_Advise)(void* _obj, void* item, void* data, int size, int format)
{
	return (int)((ELJConnection*)_obj)->Advise((wxChar*)item, (wxChar*)data, size, (wxIPCFormat)format);
}
	
EWXWEXPORT(int, ELJConnection_Disconnect)(void* _obj)
{
	return (int)((ELJConnection*)_obj)->Disconnect();
}
	
EWXWEXPORT(void, ELJConnection_Compress)(void* _obj, int on)
{
	((ELJConnection*)_obj)->Compress(on != 0);
}
	
EWXWEXPORT(void, ELJConnection_SetOnAdvise)(void* _obj, void* _fnc)
{
	((ELJConnection*)_obj)->SetOnAdvise(_fnc);
}
	
EWXWEXPORT(void, ELJConnection_SetOnExecute)(void* _obj, void* _fnc)
{
	((ELJConnection*)_obj)->SetOnExecute(_fnc);
}
	
EWXWEXPORT(void, ELJConnection_SetOnRequest)(void* _obj, void* _fnc)
{
	((ELJConnection*)_obj)->SetOnRequest(_fnc);
}
	
EWXWEXPORT(void, ELJConnection_SetOnPoke)(void* _obj, void* _fnc)
{
	((ELJConnection*)_obj)->SetOnPoke(_fnc);
}
	
EWXWEXPORT(void, ELJConnection_SetOnStartAdvise)(void* _obj, void* _fnc)
{
	((ELJConnection*)_obj)->SetOnStartAdvise(_fnc);
}
	
EWXWEXPORT(void, ELJConnection_SetOnStopAdvise)(void* _obj, void* _fnc)
{
	((ELJConnection*)_obj)->SetOnStopAdvise(_fnc);
}

EWXWEXPORT(void, ELJConnection_SetOnDisconnect)(void* _obj, void* _fnc)
{
	((ELJConnection*)_obj)->SetOnDisconnect(_fnc);
}

EWXWEXPORT(void*, ELJServer_Create)(void* _eobj, void* _cnct)
{
	return new ELJServer(_eobj, _cnct);
}

EWXWEXPORT(void, ELJServer_Delete)(void* _obj)
{
	delete (ELJServer*)_obj;
}

EWXWEXPORT(int, ELJServer_Initialize)(void* _obj, void* name)
{
	return ((ELJServer*)_obj)->Create((wxChar*)name);
}
	
EWXWEXPORT(void*, ELJClient_Create)(void* _eobj, void* _cnct)
{
	return new ELJClient(_eobj, _cnct);
}

EWXWEXPORT(void, ELJClient_Delete)(void* _obj)
{
	delete (ELJClient*)_obj;
}

EWXWEXPORT(void, ELJClient_MakeConnection)(void* _obj, void* host, void* server, void* topic)
{
	((ELJClient*)_obj)->MakeConnection((wxChar*)host, (wxChar*)server, (wxChar*)topic);
}
	
}
