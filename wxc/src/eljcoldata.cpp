#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxColourData_Create) ()
{
	return (void*) new wxColourData();
}

EWXWEXPORT(void, wxColourData_Delete) (void* _obj)
{
	delete (wxColourData*)_obj;
}

EWXWEXPORT(void, wxColourData_SetChooseFull)(void* _obj, int flag)
{
	((wxColourData*)_obj)->SetChooseFull(flag != 0);
}
	
EWXWEXPORT(int, wxColourData_GetChooseFull)(void* _obj)
{
	return (int)((wxColourData*)_obj)->GetChooseFull();
}
	
EWXWEXPORT(void, wxColourData_SetColour)(void* _obj, void* colour)
{
	((wxColourData*)_obj)->SetColour(*((wxColour*)colour));
}
	
EWXWEXPORT(void, wxColourData_GetColour)(void* _obj, void* _ref)
{
	*((wxColour*)_ref) = ((wxColourData*)_obj)->GetColour();
}
	
EWXWEXPORT(void, wxColourData_SetCustomColour)(void* _obj, int i, void* colour)
{
	((wxColourData*)_obj)->SetCustomColour(i, *((wxColour*)colour));
}
	
EWXWEXPORT(void, wxColourData_GetCustomColour)(void* _obj, int i, void* _ref)
{
	*((wxColour*)_ref) = ((wxColourData*)_obj)->GetCustomColour(i);
}
	
} 
