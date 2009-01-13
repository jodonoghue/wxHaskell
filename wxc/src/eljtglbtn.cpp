#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/tglbtn.h"

extern "C"
{

EWXWEXPORT(void*,wxToggleButton_Create)(wxWindow* parent,int id,wxString* label,int x,int y,int w,int h,int style)
{
	return (void*)new wxToggleButton(parent, (wxWindowID)id, *label, wxPoint(x, y), wxSize(w, h), (long)style);
}
	
EWXWEXPORT(void,wxToggleButton_SetValue)(void* _obj,int state)
{
	((wxToggleButton*)_obj)->SetValue(state != 0);
}
	
EWXWEXPORT(int,wxToggleButton_GetValue)(void* _obj)
{
	return (int)((wxToggleButton*)_obj)->GetValue();
}
	
EWXWEXPORT(void,wxToggleButton_SetLabel)(void* _obj,wxString* label)
{
	((wxToggleButton*)_obj)->SetLabel(*label);
}
	
EWXWEXPORT(int,wxToggleButton_Enable)(void* _obj,int enable)
{
	return (int)((wxToggleButton*)_obj)->Enable(enable != 0);
}

EWXWEXPORT(int,expEVT_COMMAND_TOGGLEBUTTON_CLICKED)()
{
	return wxEVT_COMMAND_TOGGLEBUTTON_CLICKED;
}

}
#endif
