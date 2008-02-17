#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxCalendarCtrl_Create) (void* _prt, int _id, void* _dat, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxCalendarCtrl ((wxWindow*)_prt, _id, *((wxDateTime*)_dat), wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void, wxCalendarCtrl_SetDate)(void* _obj, void* date)
{
	((wxCalendarCtrl*)_obj)->SetDate(*((wxDateTime*)date));
}
	
EWXWEXPORT(void, wxCalendarCtrl_GetDate)(void* _obj, void* date)
{
	*((wxDateTime*)date) = ((wxCalendarCtrl*)_obj)->GetDate();
}
	
EWXWEXPORT(void, wxCalendarCtrl_EnableYearChange)(void* _obj, int enable)
{
	((wxCalendarCtrl*)_obj)->EnableYearChange(enable != 0);
}
	
EWXWEXPORT(void, wxCalendarCtrl_EnableMonthChange)(void* _obj, int enable)
{
	((wxCalendarCtrl*)_obj)->EnableMonthChange(enable != 0);
}
	
EWXWEXPORT(void, wxCalendarCtrl_EnableHolidayDisplay)(void* _obj, int display)
{
	((wxCalendarCtrl*)_obj)->EnableHolidayDisplay(display != 0);
}
	
EWXWEXPORT(void, wxCalendarCtrl_SetHeaderColours)(void* _obj, void* colFg, void* colBg)
{
	((wxCalendarCtrl*)_obj)->SetHeaderColours(*((wxColour*)colFg), *((wxColour*)colBg));
}
	
EWXWEXPORT(void, wxCalendarCtrl_GetHeaderColourFg)(void* _obj, void* colour)
{
	*((wxColour*)colour) = ((wxCalendarCtrl*)_obj)->GetHeaderColourFg();
}
	
EWXWEXPORT(void, wxCalendarCtrl_GetHeaderColourBg)(void* _obj, void* colour)
{
	*((wxColour*)colour) = ((wxCalendarCtrl*)_obj)->GetHeaderColourBg();
}
	
EWXWEXPORT(void, wxCalendarCtrl_SetHighlightColours)(void* _obj, void* colFg, void* colBg)
{
	((wxCalendarCtrl*)_obj)->SetHighlightColours(*((wxColour*)colFg), *((wxColour*)colBg));
}
	
EWXWEXPORT(void, wxCalendarCtrl_GetHighlightColourFg)(void* _obj, void* colour)
{
	*((wxColour*)colour) = ((wxCalendarCtrl*)_obj)->GetHighlightColourFg();
}
	
EWXWEXPORT(void, wxCalendarCtrl_GetHighlightColourBg)(void* _obj, void* colour)
{
	*((wxColour*)colour) = ((wxCalendarCtrl*)_obj)->GetHighlightColourBg();
}
	
EWXWEXPORT(void, wxCalendarCtrl_SetHolidayColours)(void* _obj, void* colFg, void* colBg)
{
	((wxCalendarCtrl*)_obj)->SetHolidayColours(*((wxColour*)colFg), *((wxColour*)colBg));
}
	
EWXWEXPORT(void, wxCalendarCtrl_GetHolidayColourFg)(void* _obj, void* colour)
{
	*((wxColour*)colour) = ((wxCalendarCtrl*)_obj)->GetHolidayColourFg();
}
	
EWXWEXPORT(void, wxCalendarCtrl_GetHolidayColourBg)(void* _obj, void* colour)
{
	*((wxColour*)colour) = ((wxCalendarCtrl*)_obj)->GetHolidayColourBg();
}
	
EWXWEXPORT(void*, wxCalendarCtrl_GetAttr)(void* _obj, int day)
{
	return (void*)((wxCalendarCtrl*)_obj)->GetAttr((size_t)day);
}
	
EWXWEXPORT(void, wxCalendarCtrl_SetAttr)(void* _obj, int day, void* attr)
{
	((wxCalendarCtrl*)_obj)->SetAttr((size_t)day, (wxCalendarDateAttr*)attr);
}
	
EWXWEXPORT(void, wxCalendarCtrl_SetHoliday)(void* _obj, int day)
{
	((wxCalendarCtrl*)_obj)->SetHoliday((size_t)day);
}
	
EWXWEXPORT(void, wxCalendarCtrl_ResetAttr)(void* _obj, int day)
{
	((wxCalendarCtrl*)_obj)->ResetAttr((size_t)day);
}
	
EWXWEXPORT(int, wxCalendarCtrl_HitTest)(void* _obj, int x, int y, void* date, void* wd)
{
	return (int)((wxCalendarCtrl*)_obj)->HitTest(wxPoint(x, y), (wxDateTime*)date, (wxDateTime::WeekDay*)wd);
}
	

EWXWEXPORT(void*, wxCalendarDateAttr_Create)(void* _ctxt, void* _cbck, void* _cbrd, void* _fnt, int _brd)
{
	return (void*) new wxCalendarDateAttr(*((wxColour*)_ctxt), *((wxColour*)_cbck), *((wxColour*)_cbrd), *((wxFont*)_fnt), (wxCalendarDateBorder)_brd);
}

EWXWEXPORT(void*, wxCalendarDateAttr_CreateDefault)()
{
	return (void*) new wxCalendarDateAttr();
}

EWXWEXPORT(void, wxCalendarDateAttr_Delete)(void* _obj)
{
	delete (wxCalendarDateAttr*)_obj;
}

EWXWEXPORT(void, wxCalendarDateAttr_SetTextColour)(void* _obj, void* col)
{
	((wxCalendarDateAttr*)_obj)->SetTextColour(*((wxColour*)col));
}
	
EWXWEXPORT(void, wxCalendarDateAttr_SetBackgroundColour)(void* _obj, void* col)
{
	((wxCalendarDateAttr*)_obj)->SetBackgroundColour(*((wxColour*)col));
}
	
EWXWEXPORT(void, wxCalendarDateAttr_SetBorderColour)(void* _obj, void* col)
{
	((wxCalendarDateAttr*)_obj)->SetBorderColour(*((wxColour*)col));
}
	
EWXWEXPORT(void, wxCalendarDateAttr_SetFont)(void* _obj, void* font)
{
	((wxCalendarDateAttr*)_obj)->SetFont(*((wxFont*)font));
}
	
EWXWEXPORT(void, wxCalendarDateAttr_SetBorder)(void* _obj, int border)
{
	((wxCalendarDateAttr*)_obj)->SetBorder((wxCalendarDateBorder)border);
}
	
EWXWEXPORT(void, wxCalendarDateAttr_SetHoliday)(void* _obj, int holiday)
{
	((wxCalendarDateAttr*)_obj)->SetHoliday(holiday != 0);
}
	
EWXWEXPORT(int, wxCalendarDateAttr_HasTextColour)(void* _obj)
{
	return (int)((wxCalendarDateAttr*)_obj)->HasTextColour();
}
	
EWXWEXPORT(int, wxCalendarDateAttr_HasBackgroundColour)(void* _obj)
{
	return (int)((wxCalendarDateAttr*)_obj)->HasBackgroundColour();
}
	
EWXWEXPORT(int, wxCalendarDateAttr_HasBorderColour)(void* _obj)
{
	return (int)((wxCalendarDateAttr*)_obj)->HasBorderColour();
}
	
EWXWEXPORT(int, wxCalendarDateAttr_HasFont)(void* _obj)
{
	return (int)((wxCalendarDateAttr*)_obj)->HasFont();
}
	
EWXWEXPORT(int, wxCalendarDateAttr_HasBorder)(void* _obj)
{
	return (int)((wxCalendarDateAttr*)_obj)->HasBorder();
}
	
EWXWEXPORT(int, wxCalendarDateAttr_IsHoliday)(void* _obj)
{
	return (int)((wxCalendarDateAttr*)_obj)->IsHoliday();
}
	
EWXWEXPORT(void, wxCalendarDateAttr_GetTextColour)(void* _obj, void* _ref)
{
	*((wxColour*)_ref) = ((wxCalendarDateAttr*)_obj)->GetTextColour();
}
	
EWXWEXPORT(void, wxCalendarDateAttr_GetBackgroundColour)(void* _obj, void* _ref)
{
	*((wxColour*)_ref) = ((wxCalendarDateAttr*)_obj)->GetBackgroundColour();
}
	
EWXWEXPORT(void, wxCalendarDateAttr_GetBorderColour)(void* _obj, void* _ref)
{
	*((wxColour*)_ref) = ((wxCalendarDateAttr*)_obj)->GetBorderColour();
}
	
EWXWEXPORT(void, wxCalendarDateAttr_GetFont)(void* _obj, void* _ref)
{
	*((wxFont*)_ref) = ((wxCalendarDateAttr*)_obj)->GetFont();
}
	
EWXWEXPORT(int, wxCalendarDateAttr_GetBorder)(void* _obj)
{
	return (int)((wxCalendarDateAttr*)_obj)->GetBorder();
}

}
