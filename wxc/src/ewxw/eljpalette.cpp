#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxPalette_CreateDefault) ()
{
	return (void*) new wxPalette();
}

EWXWEXPORT(void*, wxPalette_CreateRGB) (int n, void* red, void* green, void* blue)
{
	return (void*) new wxPalette(n, (unsigned char*)red, (unsigned char*)green, (unsigned char*)blue);
}

EWXWEXPORT(void, wxPalette_Delete) (void* _obj)
{
	delete (wxPalette*)_obj;
}

EWXWEXPORT(int, wxPalette_GetPixel)(void* _obj, char red, char green, char blue)
{
	return ((wxPalette*)_obj)->GetPixel((unsigned char)red, (unsigned char)green, (unsigned char)blue);
}
	
EWXWEXPORT(int, wxPalette_GetRGB)(void* _obj, int pixel, void* red, void* green, void* blue)
{
	return (int)((wxPalette*)_obj)->GetRGB(pixel, (unsigned char*)red, (unsigned char*)green, (unsigned char*)blue);
}
	
EWXWEXPORT(int, wxPalette_Ok)(void* _obj)
{
	return (int)((wxPalette*)_obj)->Ok();
}
	
EWXWEXPORT(void, wxPalette_Assign)(void* _obj, void* palette)
{
	*((wxPalette*)_obj) = *((wxPalette*)palette);
}

EWXWEXPORT(int, wxPalette_IsEqual)(void* _obj, void* palette)
{
	return (int)(*((wxPalette*)_obj) == *((wxPalette*)palette));
}

}
