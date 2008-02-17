#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxScrolledWindow_Create) (void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxScrolledWindow ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void, wxScrolledWindow_SetTargetWindow)(void* _obj, void* target)
{
	((wxScrolledWindow*)_obj)->SetTargetWindow((wxWindow*)target);
}
	
EWXWEXPORT(void*, wxScrolledWindow_GetTargetWindow)(void* _obj)
{
	return (void*)((wxScrolledWindow*)_obj)->GetTargetWindow();
}
	
EWXWEXPORT(void, wxScrolledWindow_SetScrollbars)(void* _obj, int pixelsPerUnitX, int pixelsPerUnitY, int noUnitsX, int noUnitsY, int xPos, int yPos, int noRefresh)
{
	((wxScrolledWindow*)_obj)->SetScrollbars(pixelsPerUnitX, pixelsPerUnitY, noUnitsX, noUnitsY, xPos, yPos, noRefresh);
}
	
EWXWEXPORT(void, wxScrolledWindow_Scroll)(void* _obj, int x_pos, int y_pos)
{
	((wxScrolledWindow*)_obj)->Scroll(x_pos, y_pos);
}
	
EWXWEXPORT(int, wxScrolledWindow_GetScrollPageSize)(void* _obj, int orient)
{
	return ((wxScrolledWindow*)_obj)->GetScrollPageSize(orient);
}
	
EWXWEXPORT(void, wxScrolledWindow_SetScrollPageSize)(void* _obj, int orient, int pageSize)
{
	((wxScrolledWindow*)_obj)->SetScrollPageSize(orient, pageSize);
}
	
EWXWEXPORT(void, wxScrolledWindow_GetScrollPixelsPerUnit)(void* _obj, void* x_unit, void* y_unit)
{
	((wxScrolledWindow*)_obj)->GetScrollPixelsPerUnit((int*)x_unit, (int*)y_unit);
}
	
EWXWEXPORT(void, wxScrolledWindow_EnableScrolling)(void* _obj, int x_scrolling, int y_scrolling)
{
	((wxScrolledWindow*)_obj)->EnableScrolling(x_scrolling != 0, y_scrolling != 0);
}
	
EWXWEXPORT(void, wxScrolledWindow_GetViewStart)(void* _obj, void* x, void* y)
{
	((wxScrolledWindow*)_obj)->GetViewStart((int*)x, (int*)y);
}
	
EWXWEXPORT(void, wxScrolledWindow_ViewStart)(void* _obj, void* x, void* y)
{
	((wxScrolledWindow*)_obj)->GetViewStart((int*)x, (int*)y);
}
	
EWXWEXPORT(void, wxScrolledWindow_GetVirtualSize)(void* _obj, void* x, void* y)
{
	((wxScrolledWindow*)_obj)->GetVirtualSize((int*)x, (int*)y);
}
	
EWXWEXPORT(void, wxScrolledWindow_SetScale)(void* _obj, double xs, double ys)
{
	((wxScrolledWindow*)_obj)->SetScale(xs, ys);
}
	
EWXWEXPORT(double, wxScrolledWindow_GetScaleX)(void* _obj)
{
	return ((wxScrolledWindow*)_obj)->GetScaleX();
}
	
EWXWEXPORT(double, wxScrolledWindow_GetScaleY)(void* _obj)
{
	return ((wxScrolledWindow*)_obj)->GetScaleY();
}
	
EWXWEXPORT(void, wxScrolledWindow_CalcScrolledPosition)(void* _obj, int x, int y, void* xx, void* yy)
{
	((wxScrolledWindow*)_obj)->CalcScrolledPosition(x, y, (int*)xx, (int*)yy);
}
	
EWXWEXPORT(void, wxScrolledWindow_CalcUnscrolledPosition)(void* _obj, int x, int y, void* xx, void* yy)
{
	((wxScrolledWindow*)_obj)->CalcUnscrolledPosition(x, y, (int*)xx, (int*)yy);
}
	
EWXWEXPORT(void, wxScrolledWindow_AdjustScrollbars)(void* _obj)
{
	((wxScrolledWindow*)_obj)->AdjustScrollbars();
}
	
EWXWEXPORT(void, wxScrolledWindow_OnDraw)(void* _obj, void* dc)
{
	((wxScrolledWindow*)_obj)->OnDraw(*((wxDC*)dc));
}
	
EWXWEXPORT(void, wxScrolledWindow_PrepareDC)(void* _obj, void* dc)
{
	((wxScrolledWindow*)_obj)->PrepareDC(*((wxDC*)dc));
}

}
