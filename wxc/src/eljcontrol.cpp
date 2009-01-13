#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void,wxControl_SetLabel)(void* _obj,wxString* text)
{
	((wxControl*)_obj)->SetLabel(*text);
}
	
EWXWEXPORT(wxString*,wxControl_GetLabel)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxControl*)_obj)->GetLabel();
	return result;
}

EWXWEXPORT(void,wxControl_Command)(void* _obj,void* event)
{
	((wxControl*)_obj)->Command(*((wxCommandEvent*) event));
}

}
