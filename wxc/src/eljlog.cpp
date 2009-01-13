#include "wrapper.h"
#include "wx/log.h"

extern "C"
{

typedef void (*TLogFunc) (void*, int, void*, int);

}

class ELJLog : public wxLog
{
	private:
		TLogFunc func;
		void*    EiffelObject;
		
    protected:
		virtual void DoLog(wxLogLevel level, const wxChar *szString, time_t t)
                  {
                    wxString s(szString);
                    func (EiffelObject, (int)level, (void*)&s , (int)t);
                  }

	public:
		ELJLog (void* _obj, void* _fnc) : wxLog()
		{
			func = (TLogFunc)_fnc;
			EiffelObject = _obj;
		}
};

extern "C"
{

EWXWEXPORT(void*,ELJLog_Create)(void* _obj,void* _fnc)
{
	return (void*) new ELJLog(_obj, _fnc);
}

EWXWEXPORT(void,ELJLog_Delete)(void* _obj)
{
	delete (ELJLog*)_obj;
}

EWXWEXPORT(int,ELJLog_IsEnabled)(ELJLog* _obj)
{
	return (int)_obj->IsEnabled();
}
	
EWXWEXPORT(int,ELJLog_EnableLogging)(void* _obj,int doIt)
{
	return (int)((ELJLog*)_obj)->EnableLogging(doIt != 0);
}
	
EWXWEXPORT(void,ELJLog_OnLog)(void* _obj,int level,void* szString,int t)
{
	((ELJLog*)_obj)->OnLog((wxLogLevel)level, (const wxChar*)szString, (time_t)t);
}
	
EWXWEXPORT(void,ELJLog_Flush)(void* _obj)
{
	((ELJLog*)_obj)->Flush();
}
	
EWXWEXPORT(int,ELJLog_HasPendingMessages)(ELJLog* _obj)
{
	return (int)_obj->HasPendingMessages();
}
	
EWXWEXPORT(void,ELJLog_FlushActive)(void* _obj)
{
	((ELJLog*)_obj)->FlushActive();
}
	
EWXWEXPORT(void*,ELJLog_GetActiveTarget)()
{
	return (void*)ELJLog::GetActiveTarget();
}
	
EWXWEXPORT(void*,ELJLog_SetActiveTarget)(void* pLogger)
{
	return (void*)ELJLog::SetActiveTarget((wxLog*)pLogger);
}
	
EWXWEXPORT(void,ELJLog_Suspend)(void* _obj)
{
	((ELJLog*)_obj)->Suspend();
}
	
EWXWEXPORT(void,ELJLog_Resume)(void* _obj)
{
	((ELJLog*)_obj)->Resume();
}
	
EWXWEXPORT(void,ELJLog_SetVerbose)(void* _obj,int bVerbose)
{
	((ELJLog*)_obj)->SetVerbose(bVerbose != 0);
}
	
EWXWEXPORT(void,ELJLog_DontCreateOnDemand)(void* _obj)
{
	((ELJLog*)_obj)->DontCreateOnDemand();
}
	
EWXWEXPORT(void,ELJLog_SetTraceMask)(void* _obj,int ulMask)
{
	((ELJLog*)_obj)->SetTraceMask((wxTraceMask)ulMask);
}
	
EWXWEXPORT(void,ELJLog_AddTraceMask)(void* _obj,void* str)
{
	((ELJLog*)_obj)->AddTraceMask((const wxChar*)str);
}
	
EWXWEXPORT(void,ELJLog_RemoveTraceMask)(void* _obj,void* str)
{
	((ELJLog*)_obj)->RemoveTraceMask((const wxChar*)str);
}
	
EWXWEXPORT(void,ELJLog_SetTimestamp)(void* _obj,void* ts)
{
	((ELJLog*)_obj)->SetTimestamp((const wxChar*)ts);
}
	
EWXWEXPORT(int,ELJLog_GetVerbose)(ELJLog* _obj)
{
	return (int)_obj->GetVerbose();
}
	
EWXWEXPORT(int,ELJLog_GetTraceMask)(ELJLog* _obj)
{
	return (int)_obj->GetTraceMask();
}
	
EWXWEXPORT(int,ELJLog_IsAllowedTraceMask)(ELJLog* _obj,void* mask)
{
	return (int)_obj->IsAllowedTraceMask((const wxChar*)mask);
}
	
EWXWEXPORT(void*,ELJLog_GetTimestamp)(void* _obj)
{
	return (void*)((ELJLog*)_obj)->GetTimestamp();
}

EWXWEXPORT(int,ELJSysErrorCode)()
{
	return (int)wxSysErrorCode();
}

EWXWEXPORT(void*,ELJSysErrorMsg)(int nErrCode)
{
	return (void*)wxSysErrorMsg((unsigned long)nErrCode);
}

EWXWEXPORT(void,LogErrorMsg)(wxString* _msg)
{
	wxLogError(*_msg);
}

EWXWEXPORT(void,LogFatalErrorMsg)(wxString* _msg)
{
	wxLogFatalError(*_msg);
}

EWXWEXPORT(void,LogWarningMsg)(wxString* _msg)
{
	wxLogWarning(*_msg);
}

EWXWEXPORT(void,LogMessageMsg)(wxString* _msg)
{
	wxLogMessage(*_msg);
}


EWXWEXPORT(void*,wxLogChain_Create)(void* logger)
{
	return new wxLogChain ((wxLog*)logger);
}

EWXWEXPORT(void,wxLogChain_Delete)(void* _obj)
{
	delete (wxLogChain*)_obj;
}

EWXWEXPORT(void,wxLogChain_SetLog)(void* _obj,void* logger)
{
	((wxLogChain*)_obj)->SetLog((wxLog*)logger);
}
	
EWXWEXPORT(void,wxLogChain_PassMessages)(void* _obj,int bDoPass)
{
	((wxLogChain*)_obj)->PassMessages(bDoPass != 0);
}
	
EWXWEXPORT(int,wxLogChain_IsPassingMessages)(wxLogChain* _obj)
{
	return (int)_obj->IsPassingMessages();
}
	
EWXWEXPORT(void*,wxLogChain_GetOldLog)(wxLogChain* _obj)
{
	return (void*)_obj->GetOldLog();
}
	

}
