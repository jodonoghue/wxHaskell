#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxScrollBar_Create) (void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxScrollBar ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int, wxScrollBar_GetThumbPosition)(void* _obj)
{
	return ((wxScrollBar*)_obj)->GetThumbPosition();
}
	
EWXWEXPORT(int, wxScrollBar_GetThumbSize)(void* _obj)
{
	return ((wxScrollBar*)_obj)->GetThumbSize();
}
	
EWXWEXPORT(int, wxScrollBar_GetPageSize)(void* _obj)
{
	return ((wxScrollBar*)_obj)->GetPageSize();
}
	
EWXWEXPORT(int, wxScrollBar_GetRange)(void* _obj)
{
	return ((wxScrollBar*)_obj)->GetRange();
}
	
EWXWEXPORT(void, wxScrollBar_SetThumbPosition)(void* _obj, int viewStart)
{
	((wxScrollBar*)_obj)->SetThumbPosition(viewStart);
}
	
EWXWEXPORT(void, wxScrollBar_SetScrollbar)(void* _obj, int position, int thumbSize, int range, int pageSize, int refresh)
{
	((wxScrollBar*)_obj)->SetScrollbar(position, thumbSize, range, pageSize, refresh != 0);
}

}
