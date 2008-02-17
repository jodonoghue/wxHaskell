#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxPen_CreateDefault) ()
{
	return new wxPen();
}

EWXWEXPORT(void*, wxPen_CreateFromColour) (void* col, int width, int style)
{
	return new wxPen(*((wxColour*)col), width, style);
}

EWXWEXPORT(void*, wxPen_CreateFromBitmap) (void* stipple, int width)
{
#ifdef __WIN32__
	return new wxPen(*((wxBitmap*)stipple), width);
#else
	return NULL;
#endif
}

EWXWEXPORT(void*, wxPen_CreateFromStock) (int id)
{
	switch (id)
	{
		case 0:
			return (void*)wxRED_PEN;
		case 1:
			return (void*)wxCYAN_PEN;
		case 2:
			return (void*)wxGREEN_PEN;
		case 3:
			return (void*)wxBLACK_PEN;
		case 4:
			return (void*)wxWHITE_PEN;
		case 5:
			return (void*)wxTRANSPARENT_PEN;
		case 6:
			return (void*)wxBLACK_DASHED_PEN;
		case 7:
			return (void*)wxGREY_PEN;
		case 8:
			return (void*)wxMEDIUM_GREY_PEN;
		case 9:
			return (void*)wxLIGHT_GREY_PEN;
	}
	
	return NULL;
}

EWXWEXPORT(void, wxPen_Delete)(void* _obj)
{
	delete (wxPen*)_obj;
}

EWXWEXPORT(void, wxPen_Assign)(void* _obj, void* pen)
{
	*((wxPen*)_obj) = *((wxPen*)pen);
}
	
EWXWEXPORT(int, wxPen_IsEqual)(void* _obj, void* pen)
{
	return (int)(*((wxPen*)_obj) == *((wxPen*)pen));
}
	
EWXWEXPORT(int, wxPen_Ok)(void* _obj)
{
	return (int)((wxPen*)_obj)->Ok();
}
	
EWXWEXPORT(void, wxPen_SetColour)(void* _obj, void* col)
{
	((wxPen*)_obj)->SetColour(*((wxColour*)col));
}
	
EWXWEXPORT(void, wxPen_SetColourSingle)(void* _obj, char r, char g, char b)
{
	((wxPen*)_obj)->SetColour((unsigned char)r, (unsigned char)g, (unsigned char)b);
}
	
EWXWEXPORT(void, wxPen_SetWidth)(void* _obj, int width)
{
	((wxPen*)_obj)->SetWidth(width);
}
	
EWXWEXPORT(void, wxPen_SetStyle)(void* _obj, int style)
{
	((wxPen*)_obj)->SetStyle(style);
}
	
EWXWEXPORT(void, wxPen_SetStipple)(void* _obj, void* stipple)
{
#ifdef __WIN32__
	((wxPen*)_obj)->SetStipple(*((wxBitmap*)stipple));
#endif
}
	
EWXWEXPORT(void, wxPen_SetDashes)(void* _obj, int nb_dashes, void* dash)
{
	((wxPen*)_obj)->SetDashes(nb_dashes, (wxDash*)dash);
}
	
EWXWEXPORT(void, wxPen_SetJoin)(void* _obj, int join)
{
	((wxPen*)_obj)->SetJoin(join);
}
	
EWXWEXPORT(void, wxPen_SetCap)(void* _obj, int cap)
{
	((wxPen*)_obj)->SetCap(cap);
}
	
EWXWEXPORT(void, wxPen_GetColour)(void* _obj, void* _ref)
{
	*((wxColour*)_ref) = ((wxPen*)_obj)->GetColour();
}
	
EWXWEXPORT(int, wxPen_GetWidth)(void* _obj)
{
	return ((wxPen*)_obj)->GetWidth();
}
	
EWXWEXPORT(int, wxPen_GetStyle)(void* _obj)
{
	return ((wxPen*)_obj)->GetStyle();
}
	
EWXWEXPORT(int, wxPen_GetJoin)(void* _obj)
{
	return ((wxPen*)_obj)->GetJoin();
}
	
EWXWEXPORT(int, wxPen_GetCap)(void* _obj)
{
	return ((wxPen*)_obj)->GetCap();
}
	
EWXWEXPORT(int, wxPen_GetDashes)(void* _obj, void* ptr)
{
	return ((wxPen*)_obj)->GetDashes((wxDash**)ptr);
}
	
EWXWEXPORT(void, wxPen_GetStipple)(void* _obj, void* _ref)
{
#ifdef __WIN32__
	*((wxBitmap*)_ref) = *(((wxPen*)_obj)->GetStipple());
#endif
}
	
}
