#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void, wxControl_SetLabel)(void* _obj, char* text)
{
	((wxControl*)_obj)->SetLabel(text);
}
	
EWXWEXPORT(int, wxControl_GetLabel)(void* _obj, void* _buf)
{
	wxString result = ((wxControl*)_obj)->GetLabel();
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}

EWXWEXPORT(void, wxControl_Command)(void* _obj, void* event)
{
	((wxControl*)_obj)->Command(*((wxCommandEvent*) event));
}

}
