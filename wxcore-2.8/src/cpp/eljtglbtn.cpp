#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/tglbtn.h"

extern "C"
{

EWXWEXPORT(void*,wxToggleButton_Create)(wxWindow* parent,int id,wxString* label,int x,int y,int w,int h,int style)
{
	return (void*)new wxToggleButton(parent, (wxWindowID)id, *label, wxPoint(x, y), wxSize(w, h), (long)style);
}
	
EWXWEXPORT(void,wxToggleButton_SetValue)(wxToggleButton* self,bool state)
{
	self->SetValue(state);
}
	
EWXWEXPORT(bool,wxToggleButton_GetValue)(wxToggleButton* self)
{
	return self->GetValue();
}
	
EWXWEXPORT(void,wxToggleButton_SetLabel)(wxToggleButton* self,wxString* label)
{
	self->SetLabel(*label);
}
	
EWXWEXPORT(bool,wxToggleButton_Enable)(wxToggleButton* self,bool enable)
{
	return self->Enable(enable);
}

EWXWEXPORT(int,expEVT_COMMAND_TOGGLEBUTTON_CLICKED)()
{
	return wxEVT_COMMAND_TOGGLEBUTTON_CLICKED;
}

}
#endif
