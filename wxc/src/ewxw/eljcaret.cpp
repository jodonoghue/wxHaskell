#include "wrapper.h"
#include "wx/caret.h"

extern "C"
{

EWXWEXPORT(void*, wxCaret_Create)(void* _wnd, int _wth, int _hgt)
{
	return new wxCaret((wxWindow*)_wnd, _wth, _hgt);
}

EWXWEXPORT(int,wxCaret_IsOk)(void* _obj)
{
	return (int)((wxCaret*)_obj)->IsOk();
}
	
EWXWEXPORT(int,wxCaret_IsVisible)(void* _obj)
{
	return (int)((wxCaret*)_obj)->IsVisible();
}
	
EWXWEXPORT(void,wxCaret_GetPosition)(void* _obj, void* x, void* y)
{
	((wxCaret*)_obj)->GetPosition((int*)x, (int*)y);
}
	
EWXWEXPORT(void,wxCaret_GetSize)(void* _obj, void* width, void* height)
{
	((wxCaret*)_obj)->GetSize((int*)width, (int*)height);
}
	
EWXWEXPORT(void*,wxCaret_GetWindow)(void* _obj)
{
	return (void*)((wxCaret*)_obj)->GetWindow();
}
	
EWXWEXPORT(void,wxCaret_SetSize)(void* _obj, int width, int height)
{
	((wxCaret*)_obj)->SetSize(width, height);
}
	
EWXWEXPORT(void,wxCaret_Move)(void* _obj, int x, int y)
{
	((wxCaret*)_obj)->Move(x, y);
}
	
EWXWEXPORT(void,wxCaret_Show)(void* _obj)
{
	((wxCaret*)_obj)->Show();
}
	
EWXWEXPORT(void,wxCaret_Hide)(void* _obj)
{
	((wxCaret*)_obj)->Hide();
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
