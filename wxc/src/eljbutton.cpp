#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxButton_Create)(wxWindow* _prt,int _id,wxString* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxButton (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(bool,wxButton_SetBackgroundColour)(wxButton* _obj,wxColour* colour)
{
	return _obj->SetBackgroundColour(*colour);
}
	
EWXWEXPORT(void,wxButton_SetDefault)(wxButton* _obj)
{
	_obj->SetDefault();
}

EWXWEXPORT(void*,wxBitmapButton_Create)(wxWindow* _prt,int _id,void* _bmp, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxBitmapButton (_prt, _id, *((wxBitmap*)_bmp), wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(void,wxBitmapButton_GetBitmapLabel)(wxBitmapButton* _obj,void* _ref)
{
	*((wxBitmap*)_ref) = _obj->GetBitmapLabel();
}
	
EWXWEXPORT(void,wxBitmapButton_GetBitmapSelected)(wxBitmapButton* _obj,void* _ref)
{
	*((wxBitmap*)_ref) = _obj->GetBitmapSelected();
}
	
EWXWEXPORT(void,wxBitmapButton_GetBitmapFocus)(wxBitmapButton* _obj,void* _ref)
{
	*((wxBitmap*)_ref) = _obj->GetBitmapFocus();
}
	
EWXWEXPORT(void,wxBitmapButton_GetBitmapDisabled)(wxBitmapButton* _obj,void* _ref)
{
	*((wxBitmap*)_ref) = _obj->GetBitmapDisabled();
}
	
EWXWEXPORT(void,wxBitmapButton_SetBitmapSelected)(wxBitmapButton* _obj,void* sel)
{
	_obj->SetBitmapSelected(*((wxBitmap*)sel));
}
	
EWXWEXPORT(void,wxBitmapButton_SetBitmapFocus)(wxBitmapButton* _obj,void* focus)
{
	_obj->SetBitmapFocus(*((wxBitmap*)focus));
}
	
EWXWEXPORT(void,wxBitmapButton_SetBitmapDisabled)(wxBitmapButton* _obj,void* disabled)
{
	_obj->SetBitmapDisabled(*((wxBitmap*)disabled));
}
	
EWXWEXPORT(void,wxBitmapButton_SetBitmapLabel)(wxBitmapButton* _obj,void* bitmap)
{
	_obj->SetBitmapLabel(*((wxBitmap*)bitmap));
}
	
EWXWEXPORT(void,wxBitmapButton_SetMargins)(wxBitmapButton* _obj,int x,int y)
{
	_obj->SetMargins(x, y);
}
	
EWXWEXPORT(int,wxBitmapButton_GetMarginX)(wxBitmapButton* _obj)
{
	return _obj->GetMarginX();
}
	
EWXWEXPORT(int,wxBitmapButton_GetMarginY)(wxBitmapButton* _obj)
{
	return _obj->GetMarginY();
}
	
}
