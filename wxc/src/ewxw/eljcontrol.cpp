#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void, wxControl_SetLabel)(void* _obj, wxChar* text)
{
	((wxControl*)_obj)->SetLabel(text);
}
	
EWXWEXPORT(int, wxControl_GetLabel)(void* _obj, void* _buf)
{
	wxString result = ((wxControl*)_obj)->GetLabel();
	return copyStrToBuf(_buf, result); 
}

EWXWEXPORT(void, wxControl_Command)(void* _obj, void* event)
{
	((wxControl*)_obj)->Command(*((wxCommandEvent*) event));
}

}
