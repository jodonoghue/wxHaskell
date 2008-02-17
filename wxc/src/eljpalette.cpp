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
#if (wxVERSION_NUMBER <= 2800)
	return (int)(*((wxPalette*)_obj) == *((wxPalette*)palette));
#else
	wxPalette* pal1 = (wxPalette *)_obj;
	wxPalette* pal2 = (wxPalette *)palette;
	if (pal1->GetColoursCount() == pal2->GetColoursCount()){
		bool equal = true;
		unsigned char red1 = 0;
		unsigned char red2 = 0;
		unsigned char green1 = 0;
		unsigned char green2 = 0;
		unsigned char blue1 = 0;
		unsigned char blue2 = 0;
		for(int x = 0; x<(pal1->GetColoursCount()); x++){
			pal1->GetRGB(x, &red1, &green1, &blue1);
			pal2->GetRGB(x, &red2, &green2, &blue2);
			equal = equal && (red1==red2 && green1==green2 && blue1==blue2); 
		}
		return (int)equal;
	} else {
		return 0;
	}
#endif
}

}
