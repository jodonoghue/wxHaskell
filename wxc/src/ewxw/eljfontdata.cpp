#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxFontData_Create) ()
{
	return (void*) new wxFontData();
}

EWXWEXPORT(void, wxFontData_Delete) (void* _obj)
{
	delete (wxFontData*)_obj;
}

EWXWEXPORT(void, wxFontData_SetAllowSymbols)(void* _obj, int flag)
{
	((wxFontData*)_obj)->SetAllowSymbols(flag != 0);
}
	
EWXWEXPORT(int, wxFontData_GetAllowSymbols)(void* _obj)
{
	return (int)((wxFontData*)_obj)->GetAllowSymbols();
}
	
EWXWEXPORT(void, wxFontData_SetColour)(void* _obj, void* colour)
{
	((wxFontData*)_obj)->SetColour(*((wxColour*)colour));
}
	
EWXWEXPORT(void, wxFontData_GetColour)(void* _obj, void* _ref)
{
	*((wxColour*)_ref) = ((wxFontData*)_obj)->GetColour();
}
	
EWXWEXPORT(void, wxFontData_SetShowHelp)(void* _obj, int flag)
{
	((wxFontData*)_obj)->SetShowHelp(flag != 0);
}
	
EWXWEXPORT(int, wxFontData_GetShowHelp)(void* _obj)
{
	return (int)((wxFontData*)_obj)->GetShowHelp();
}
	
EWXWEXPORT(void, wxFontData_EnableEffects)(void* _obj, int flag)
{
	((wxFontData*)_obj)->EnableEffects(flag != 0);
}
	
EWXWEXPORT(int, wxFontData_GetEnableEffects)(void* _obj)
{
	return (int)((wxFontData*)_obj)->GetEnableEffects();
}
	
EWXWEXPORT(void, wxFontData_SetInitialFont)(void* _obj, void* font)
{
	((wxFontData*)_obj)->SetInitialFont(*((wxFont*)font));
}
	
EWXWEXPORT(void, wxFontData_GetInitialFont)(void* _obj, void* ref)
{
	*((wxFont*)ref) = ((wxFontData*)_obj)->GetInitialFont();
}
	
EWXWEXPORT(void, wxFontData_SetChosenFont)(void* _obj, void* font)
{
	((wxFontData*)_obj)->SetChosenFont(*((wxFont*)font));
}
	
EWXWEXPORT(void, wxFontData_GetChosenFont)(void* _obj, void* ref)
{
	*((wxFont*)ref) = ((wxFontData*)_obj)->GetChosenFont();
}
	
EWXWEXPORT(void, wxFontData_SetRange)(void* _obj, int minRange, int maxRange)
{
	((wxFontData*)_obj)->SetRange(minRange, maxRange);
}
	
EWXWEXPORT(int, wxFontData_GetEncoding)(void* _obj)
{
	return (int)((wxFontData*)_obj)->GetEncoding();
}
	
EWXWEXPORT(void, wxFontData_SetEncoding)(void* _obj, int encoding)
{
	((wxFontData*)_obj)->SetEncoding((wxFontEncoding)encoding);
}
	
}
