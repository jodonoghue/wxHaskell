#include "wrapper.h"
#include "wx/tooltip.h"
#include "wx/dynlib.h"
#include "wx/quantize.h"
#include "wx/fs_zip.h"

AppInitFunc init_func = NULL;
void* eiffel_obj = NULL;
int APPTerminating = 0;

#ifdef __WIN32__
IMPLEMENT_APP(ELJApp);
#else
IMPLEMENT_APP_NO_MAIN(ELJApp);
#endif

bool ELJApp::OnInit (void)
{
	return init_func (eiffel_obj) != 0;
}

void ELJApp::HandleEvent(wxEvent& _evt)
{
	EiffelCallback* call_back = (EiffelCallback*)((ELJCallback*)_evt.m_callbackUserData)->call_back;
	call_back->event = (void*)&_evt;
	call_back->func(call_back->object, call_back->data);
}

void ELJApp::InitZipFileSystem()
{
	static int InitZIPFileSystem_done = 0;
	
	if (!InitZIPFileSystem_done)
	{
		InitZIPFileSystem_done = 1;
		wxFileSystem::AddHandler(new wxZipFSHandler());
	}
}

void ELJApp::InitImageHandlers()
{
	static int InitImageHandlers_done = 0;
	
	if (!InitImageHandlers_done)
	{
		InitImageHandlers_done = 1;
		wxInitAllImageHandlers();
	}
}

extern "C"
{

//int OnExit();
//virtual void OnFatalException();
EWXWEXPORT(int, ELJApp_MainLoop)()
{
	return wxGetApp().MainLoop();
}
	
EWXWEXPORT(int, ELJApp_Initialized)()
{
	return (int)wxGetApp().Initialized();
}
	
EWXWEXPORT(int, ELJApp_Pending)()
{
	return (int)wxGetApp().Pending();
}
	
EWXWEXPORT(void, ELJApp_Dispatch)()
{
	wxGetApp().Dispatch();
}
	
EWXWEXPORT(int, ELJApp_GetAppName)(void* _buf)
{
	wxString result = wxGetApp().GetAppName();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(void, ELJApp_SetAppName)(char* name)
{
	wxGetApp().SetAppName(name);
}
	
EWXWEXPORT(int, ELJApp_GetClassName)(void* _buf)
{
	wxString result = wxGetApp().GetClassName();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(void, ELJApp_SetClassName)(char* name)
{
	wxGetApp().SetClassName(name);
}
	
EWXWEXPORT(int, ELJApp_GetVendorName)(void* _buf)
{
	wxString result = wxGetApp().GetVendorName();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(void, ELJApp_SetVendorName)(char* name)
{
	wxGetApp().SetVendorName(name);
}
	
EWXWEXPORT(void*, ELJApp_GetTopWindow)()
{
	return wxGetApp().GetTopWindow();
}

EWXWEXPORT(void, ELJApp_SetExitOnFrameDelete)(int flag)
{
	wxGetApp().SetExitOnFrameDelete(flag != 0);
}
	
EWXWEXPORT(int, ELJApp_GetExitOnFrameDelete)()
{
	return (int)wxGetApp().GetExitOnFrameDelete();
}
	
EWXWEXPORT(void*, ELJApp_CreateLogTarget)()
{
	return wxGetApp().CreateLogTarget();
}
	
EWXWEXPORT(int, ELJApp_GetWantDebugOutput)()
{
#if wxVERSION_NUMBER < 2400
	return (int)wxGetApp().GetWantDebugOutput();
#else
	return 0;
#endif
}
	
EWXWEXPORT(void, ELJApp_SetUseBestVisual)( int flag )
{
	wxGetApp().SetUseBestVisual( flag != 0 );
}
	
EWXWEXPORT(int, ELJApp_GetUseBestVisual)()
{
	return (int)wxGetApp().GetUseBestVisual();
}
	
EWXWEXPORT(void, ELJApp_SetPrintMode)(int mode)
{
	wxGetApp().SetPrintMode(mode);
}
	
EWXWEXPORT(void, ELJApp_ExitMainLoop) ()
{
	wxGetApp ().ExitMainLoop();
}

EWXWEXPORT(void, ELJApp_SetTopWindow) (void* _wnd)
{
	wxGetApp ().SetTopWindow ((wxWindow*)_wnd);
}

EWXWEXPORT(int, ELJApp_SendIdleEvents)()
{
	return (int)wxGetApp().SendIdleEvents();
}
	
EWXWEXPORT(int, ELJApp_SendIdleEventsToWindow)(void* win)
{
	return (int)wxGetApp().SendIdleEvents((wxWindow*) win);
}

EWXWEXPORT(void, ELJApp_EnableTooltips)(int _enable)
{
	wxToolTip::Enable (_enable != 0);
}

EWXWEXPORT(void, ELJApp_SetTooltipDelay)(int _ms)
{
	wxToolTip::SetDelay (_ms);
}

EWXWEXPORT(void, ELJApp_InitAllImageHandlers)()
{
	wxGetApp().InitImageHandlers();
}

EWXWEXPORT(void, ELJApp_Bell)()
{
	wxBell();
}

EWXWEXPORT(void, ELJApp_DisplaySize)(void* w, void* h)
{
	wxDisplaySize((int*)w, (int*)h);
}

EWXWEXPORT(void, ELJApp_EnableTopLevelWindows)(int _enb)
{
	wxEnableTopLevelWindows(_enb != 0);
}

EWXWEXPORT(void, ELJApp_Exit)()
{
	wxExit();
}

EWXWEXPORT(void, ELJApp_MousePosition)(void* x, void* y)
{
	wxGetMousePosition((int*)x, (int*)y);
}

EWXWEXPORT(void*, ELJApp_FindWindowByLabel)(void* _lbl, void* _prt)
{
	return (void*)wxWindow::FindWindowByLabel((char*)_lbl, (wxWindow*)_prt);
}

EWXWEXPORT(void*, ELJApp_FindWindowByName)(void* _lbl, void* _prt)
{
	return (void*)wxWindow::FindWindowByName((char*)_lbl, (wxWindow*)_prt);
}

EWXWEXPORT(void*, ELJApp_FindWindowById)(int _id, void* _prt)
{
	return (void*)wxWindow::FindWindowById((long)_id, (wxWindow*)_prt);
}

EWXWEXPORT(void*, ELJApp_GetApp)()
{
	return (void*)wxTheApp;
}

EWXWEXPORT(int, ELJApp_GetUserId)(void* _buf)
{
	wxString result = wxGetUserId();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}

EWXWEXPORT(int, ELJApp_GetUserName)(void* _buf)
{
	wxString result = wxGetUserName();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}

EWXWEXPORT(int, ELJApp_GetUserHome)(void* _usr, void* _buf)
{
	wxString result = wxGetUserHome((const char*)_usr);
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}

EWXWEXPORT(int, ELJApp_ExecuteProcess)(void* _cmd, int _snc, void* _prc)
{
	return (int)wxExecute((const char*)_cmd, _snc != 0, (wxProcess*)_prc);
}

EWXWEXPORT(int, ELJApp_Yield)()
{
	return (int)wxYield();
}

EWXWEXPORT(int, ELJApp_SafeYield)(void* _win)
{
	return (int)wxSafeYield((wxWindow*)_win);
}

EWXWEXPORT(int, ELJApp_GetOsVersion)(void* _maj, void* _min)
{
	return wxGetOsVersion((int*)_maj, (int*)_min);
}

EWXWEXPORT(int, ELJApp_GetOsDescription)(void* _buf)
{
	wxString result = wxGetOsDescription();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}

EWXWEXPORT(void, ELJApp_Sleep)(int _scs)
{
	wxSleep(_scs);
}

EWXWEXPORT(void, ELJApp_USleep)(int _mscs)
{
	wxUsleep(_mscs);
}

EWXWEXPORT(int,ELJApp_IsTerminating)()
{
	return APPTerminating;
}

EWXWEXPORT(int, QuantizePalette)(void* src, void* dest, void* pPalette, int desiredNoColours, void* eightBitData, int flags)
{
	return (int)wxQuantize::Quantize(*((wxImage*)src), *((wxImage*)dest), (wxPalette**)pPalette, desiredNoColours, (unsigned char**)eightBitData, flags);
}

EWXWEXPORT(int, Quantize)(void* src, void* dest, int desiredNoColours, void* eightBitData, int flags)
{
	return (int)wxQuantize::Quantize(*((wxImage*)src), *((wxImage*)dest), desiredNoColours, (unsigned char**)eightBitData, flags);
}


EWXWEXPORT(void*, wxEvtHandler_Create)()
{
	return (void*) new wxEvtHandler();
}

EWXWEXPORT(void, wxEvtHandler_Delete)(void* _obj)
{
	delete (wxEvtHandler*)_obj;
}

EWXWEXPORT(int, wxEvtHandler_Connect)(void* _obj, int first, int last, int type, void* data)
{
	void* result = (void*)new ELJCallback((EiffelCallback*)data);
	((wxEvtHandler*)_obj)->Connect(first, last, type, (wxObjectEventFunction)&ELJApp::HandleEvent, (wxObject*)result);

	return (int)result;
}

EWXWEXPORT(int, wxEvtHandler_Disconnect)(void* _obj, int first, int last, int type, int data)
{
	return (int)((wxEvtHandler*)_obj)->Disconnect(first, last, type, (wxObjectEventFunction)&ELJApp::HandleEvent, (wxObject*) data);
}

EWXWEXPORT(void*, wxEvtHandler_GetNextHandler)(void* _obj)
{
	return (void*)((wxEvtHandler*)_obj)->GetNextHandler();
}
	
EWXWEXPORT(void*, wxEvtHandler_GetPreviousHandler)(void* _obj)
{
	return (void*)((wxEvtHandler*)_obj)->GetPreviousHandler();
}
	
EWXWEXPORT(void, wxEvtHandler_SetNextHandler)(void* _obj, void* handler)
{
	((wxEvtHandler*)_obj)->SetNextHandler((wxEvtHandler*)handler);
}
	
EWXWEXPORT(void, wxEvtHandler_SetPreviousHandler)(void* _obj, void* handler)
{
	((wxEvtHandler*)_obj)->SetPreviousHandler((wxEvtHandler*)handler);
}
	
EWXWEXPORT(void, wxEvtHandler_SetEvtHandlerEnabled)(void* _obj, int enabled)
{
	((wxEvtHandler*)_obj)->SetEvtHandlerEnabled(enabled != 0);
}
	
EWXWEXPORT(int, wxEvtHandler_GetEvtHandlerEnabled)(void* _obj)
{
	return (int)((wxEvtHandler*)_obj)->GetEvtHandlerEnabled();
}
	
EWXWEXPORT(int, wxEvtHandler_ProcessEvent)(void* _obj, void* event)
{
	return (int)((wxEvtHandler*)_obj)->ProcessEvent(*((wxEvent*)event));
}
	
EWXWEXPORT(void, wxEvtHandler_AddPendingEvent)(void* _obj, void* event)
{
	((wxEvtHandler*)_obj)->AddPendingEvent(*((wxEvent*)event));
}
	
EWXWEXPORT(void, wxEvtHandler_ProcessPendingEvents)(void* _obj)
{
	((wxEvtHandler*)_obj)->ProcessPendingEvents();
}

EWXWEXPORT(void*, Null_AcceleratorTable)()
{
	return (void*)&wxNullAcceleratorTable;
}

EWXWEXPORT(void*, Null_Bitmap)()
{
	return (void*)&wxNullBitmap;
}

EWXWEXPORT(void*, Null_Icon)()
{
	return (void*)&wxNullIcon;
}

EWXWEXPORT(void*, Null_Cursor)()
{
	return (void*)&wxNullCursor;
}

EWXWEXPORT(void*, Null_Pen)()
{
	return (void*)&wxNullPen;
}

EWXWEXPORT(void*, Null_Brush)()
{
	return (void*)&wxNullBrush;
}

EWXWEXPORT(void*, Null_Palette)()
{
	return (void*)&wxNullPalette;
}

EWXWEXPORT(void*, Null_Font)()
{
	return (void*)&wxNullFont;
}

EWXWEXPORT(void*, Null_Colour)()
{
	return (void*)&wxNullColour;
}

EWXWEXPORT(int, wxDllLoader_LoadLibrary)(void* _name, void* _success)
{
#if wxVERSION_NUMBER < 2400
	bool success;
	
	wxDllType result = wxDllLoader::LoadLibrary ((const char*)_name, &success);
	
	if (success)
		*((int*)_success) = 1;
	else
		*((int*)_success) = 0;
	
	return (int) result;
#else
	wxDynamicLibrary* result = new wxDynamicLibrary((const char*)_name);
	
	if (result->IsLoaded())
		*((int*)_success) = 1;
	else
		*((int*)_success) = 0;
	
	return (int) result;
#endif
}

EWXWEXPORT(void, wxDllLoader_UnloadLibrary)(int _handle)
{
#if wxVERSION_NUMBER < 2400
	wxDllLoader::UnloadLibrary ((wxDllType)_handle);
#else
	wxDynamicLibrary* lib = (wxDynamicLibrary*)_handle;
	lib->Unload();
	delete lib;
#endif
}

EWXWEXPORT(void*, wxDllLoader_GetSymbol)(int _handle, void* _name)
{
#if wxVERSION_NUMBER < 2400
	return wxDllLoader::GetSymbol ((wxDllType)_handle, (const char*)_name);
#else
	return  ((wxDynamicLibrary*)_handle)->GetSymbol((const char*)_name);
#endif
}

EWXWEXPORT(void, wxCFree) (void* _ptr)
{
	free (_ptr);
}

EWXWEXPORT(void*, wxClassInfo_CreateClassByName) (void* _inf)
{
	wxClassInfo* inf = wxClassInfo::FindClass ((char*)_inf);
	if (inf)
		return inf->CreateObject();
	return NULL;
}

EWXWEXPORT(void*, wxClassInfo_GetClassName) (void* _obj)
{
	wxClassInfo* inf = ((wxObject*)_obj)->GetClassInfo();
	if (inf)
		return (void*)inf->GetClassName();
	return NULL;
}

EWXWEXPORT(int, wxClassInfo_IsKindOf) (void* _obj, void* _name)
{
	wxClassInfo* inf = wxClassInfo::FindClass ((char*)_name);
	if (inf)
		return (int)((wxObject*)_obj)->IsKindOf(inf);
	return 0;
}

EWXWEXPORT(int,wxEvent_NewEventType)()
{
	return (int)wxNewEventType();
}

}
