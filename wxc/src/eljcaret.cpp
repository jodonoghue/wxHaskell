#include "wrapper.h"
#include "wx/caret.h"

extern "C"
{

EWXWEXPORT(wxCaret*,wxCaret_Create)(wxWindow* _wnd,int _wth,int _hgt)
{
	return new wxCaret(_wnd, _wth, _hgt);
}

EWXWEXPORT(bool,wxCaret_IsOk)(wxCaret* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(bool,wxCaret_IsVisible)(wxCaret* self)
{
	return self->IsVisible();
}
	
EWXWEXPORT(void,wxCaret_GetPosition)(void* _obj, void* x, void* y)
{
	((wxCaret*)_obj)->GetPosition((int*)x, (int*)y);
}
	
EWXWEXPORT(void,wxCaret_GetSize)(void* _obj, void* width, void* height)
{
	((wxCaret*)_obj)->GetSize((int*)width, (int*)height);
}
	
EWXWEXPORT(wxWindow*,wxCaret_GetWindow)(wxCaret* self)
{
	return self->GetWindow();
}
	
EWXWEXPORT(void,wxCaret_SetSize)(wxCaret* self,int width,int height)
{
	self->SetSize(width, height);
}
	
EWXWEXPORT(void,wxCaret_Move)(wxCaret* self,int x,int y)
{
	self->Move(x, y);
}
	
EWXWEXPORT(void,wxCaret_Show)(wxCaret* self)
{
	self->Show();
}
	
EWXWEXPORT(void,wxCaret_Hide)(wxCaret* self)
{
	self->Hide();
}
	
EWXWEXPORT(int,wxCaret_GetBlinkTime)()
{
	return wxCaret::GetBlinkTime();
}
	
EWXWEXPORT(void,wxCaret_SetBlinkTime)(int milliseconds)
{
	wxCaret::SetBlinkTime(milliseconds);
}
	
}
