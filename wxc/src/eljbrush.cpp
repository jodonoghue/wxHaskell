#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxBrush_CreateDefault) ()
{
	return (void*) new wxBrush();
}

EWXWEXPORT(void*, wxBrush_CreateFromBitmap) (void* bitmap)
{
	return (void*) new wxBrush(*((wxBitmap*)bitmap));
}

EWXWEXPORT(void*, wxBrush_CreateFromColour) (void* col, int style)
{
	return (void*) new wxBrush(*((wxColour*)col), style);
}

EWXWEXPORT(void*, wxBrush_CreateFromStock) (int id)
{
	switch (id)
	{
		case 0:
			return (void*)wxBLUE_BRUSH;
		case 1:
			return (void*)wxGREEN_BRUSH;
		case 2:
			return (void*)wxWHITE_BRUSH;
		case 3:
			return (void*)wxBLACK_BRUSH;
		case 4:
			return (void*)wxGREY_BRUSH;
		case 5:
			return (void*)wxMEDIUM_GREY_BRUSH;
		case 6:
			return (void*)wxLIGHT_GREY_BRUSH;
		case 7:
			return (void*)wxTRANSPARENT_BRUSH;
		case 8:
			return (void*)wxCYAN_BRUSH;
		case 9:
			return (void*)wxRED_BRUSH;
	}
	
	return NULL;
}

EWXWEXPORT(void, wxBrush_Delete) (void* _obj)
{
	delete (wxBrush*)_obj;
}

EWXWEXPORT(void, wxBrush_SetColour)(void* _obj, void* col)
{
	((wxBrush*)_obj)->SetColour(*((wxColour*)col));
}
	
EWXWEXPORT(void, wxBrush_SetColourSingle)(void* _obj, char r, char g, char b)
{
	((wxBrush*)_obj)->SetColour((unsigned char)r, (unsigned char)g, (unsigned char)b);
}
	
EWXWEXPORT(void, wxBrush_SetStyle)(void* _obj, int style)
{
	((wxBrush*)_obj)->SetStyle(style);
}
	
EWXWEXPORT(void, wxBrush_SetStipple)(void* _obj, void* stipple)
{
	((wxBrush*)_obj)->SetStipple(*((wxBitmap*)stipple));
}
	
EWXWEXPORT(void, wxBrush_Assign)(void* _obj, void* brush)
{
	*((wxBrush*)_obj) = *((wxBrush*)brush);
}
	
EWXWEXPORT(int, wxBrush_IsEqual)(void* _obj, void* brush)
{
	return (int)(*((wxBrush*)_obj) == *((wxBrush*)brush));
}
	
EWXWEXPORT(void, wxBrush_GetColour)(void* _obj, void* _ref)
{
	*((wxColour*)_ref) = ((wxBrush*)_obj)->GetColour();
}
	
EWXWEXPORT(int, wxBrush_GetStyle)(void* _obj)
{
	return ((wxBrush*)_obj)->GetStyle();
}
	
EWXWEXPORT(void, wxBrush_GetStipple)(void* _obj, void* _ref)
{
	*((wxBitmap*)_ref) = (*((wxBrush*)_obj)->GetStipple());
}
	
EWXWEXPORT(int, wxBrush_Ok)(void* _obj)
{
	return (int)((wxBrush*)_obj)->Ok();
}
	
}
