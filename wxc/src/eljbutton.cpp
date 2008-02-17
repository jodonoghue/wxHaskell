#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxButton_Create) (void* _prt, int _id, wxChar* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxButton ((wxWindow*)_prt, _id, _txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(int, wxButton_SetBackgroundColour)(void* _obj, void* colour)
{
	return (int)((wxButton*)_obj)->SetBackgroundColour(*((wxColour*)colour));
}
	
EWXWEXPORT(void, wxButton_SetDefault)(void* _obj)
{
	((wxButton*)_obj)->SetDefault();
}

EWXWEXPORT(void*, wxBitmapButton_Create) (void* _prt, int _id, void* _bmp, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxBitmapButton ((wxWindow*)_prt, _id, *((wxBitmap*)_bmp), wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(void, wxBitmapButton_GetBitmapLabel)(void* _obj, void* _ref)
{
	*((wxBitmap*)_ref) = ((wxBitmapButton*)_obj)->GetBitmapLabel();
}
	
EWXWEXPORT(void, wxBitmapButton_GetBitmapSelected)(void* _obj, void* _ref)
{
	*((wxBitmap*)_ref) = ((wxBitmapButton*)_obj)->GetBitmapSelected();
}
	
EWXWEXPORT(void, wxBitmapButton_GetBitmapFocus)(void* _obj, void* _ref)
{
	*((wxBitmap*)_ref) = ((wxBitmapButton*)_obj)->GetBitmapFocus();
}
	
EWXWEXPORT(void, wxBitmapButton_GetBitmapDisabled)(void* _obj, void* _ref)
{
	*((wxBitmap*)_ref) = ((wxBitmapButton*)_obj)->GetBitmapDisabled();
}
	
EWXWEXPORT(void, wxBitmapButton_SetBitmapSelected)(void* _obj, void* sel)
{
	((wxBitmapButton*)_obj)->SetBitmapSelected(*((wxBitmap*)sel));
}
	
EWXWEXPORT(void, wxBitmapButton_SetBitmapFocus)(void* _obj, void* focus)
{
	((wxBitmapButton*)_obj)->SetBitmapFocus(*((wxBitmap*)focus));
}
	
EWXWEXPORT(void, wxBitmapButton_SetBitmapDisabled)(void* _obj, void* disabled)
{
	((wxBitmapButton*)_obj)->SetBitmapDisabled(*((wxBitmap*)disabled));
}
	
EWXWEXPORT(void, wxBitmapButton_SetBitmapLabel)(void* _obj, void* bitmap)
{
	((wxBitmapButton*)_obj)->SetBitmapLabel(*((wxBitmap*)bitmap));
}
	
EWXWEXPORT(void, wxBitmapButton_SetMargins)(void* _obj, int x, int y)
{
	((wxBitmapButton*)_obj)->SetMargins(x, y);
}
	
EWXWEXPORT(int, wxBitmapButton_GetMarginX)(void* _obj)
{
	return ((wxBitmapButton*)_obj)->GetMarginX();
}
	
EWXWEXPORT(int, wxBitmapButton_GetMarginY)(void* _obj)
{
	return ((wxBitmapButton*)_obj)->GetMarginY();
}
	
}
