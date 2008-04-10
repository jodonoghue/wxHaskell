#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxColour_CreateEmpty) ()
{
	return (void*) new wxColour();
}

EWXWEXPORT(void*, wxColour_CreateRGB) (char _red, char _green, char _blue)
{
	return (void*) new wxColour(_red, _green, _blue);
}

EWXWEXPORT(void*, wxColour_CreateByName) (void* _name)
{
	return (void*) new wxColour((wxChar*)_name);
}

EWXWEXPORT(void*, wxColour_CreateFromStock) (int _id)
{
	switch (_id)
	{
		case 0:
			return (void*)wxBLACK;
		case 1:
			return (void*)wxWHITE;
		case 2:
			return (void*)wxRED;
		case 3:
			return (void*)wxBLUE;
		case 4:
			return (void*)wxGREEN;
		case 5:
			return (void*)wxCYAN;
		case 6:
			return (void*)wxLIGHT_GREY;
	}

	return NULL;
}

EWXWEXPORT(void, wxColour_Delete)(void* _obj)
{
	delete (wxColour*)_obj;
}

EWXWEXPORT(void, wxColour_Set)(void* _obj, char _red, char _green, char _blue)
{
	((wxColour*)_obj)->Set(_red, _green, _blue);
}
	
EWXWEXPORT(void, wxColour_Assign)(void* _obj, void* other)
{
	*((wxColour*)_obj) = *((wxColour*)other);
}
	
EWXWEXPORT(int, wxColour_Ok)(void* _obj)
{
	return (int)((wxColour*)_obj)->Ok();
}
	
EWXWEXPORT(char, wxColour_Red)(void* _obj)
{
	return ((wxColour*)_obj)->Red();
}
	
EWXWEXPORT(char, wxColour_Green)(void* _obj)
{
	return ((wxColour*)_obj)->Green();
}
	
EWXWEXPORT(char, wxColour_Blue)(void* _obj)
{
	return ((wxColour*)_obj)->Blue();
}

// FIXME: the return type on this is platform dependent
// and thus evil.  If you really want a GetPixel method,
// please hack this code and throw in the relevant 
// ifdefs, cuz I don't want to deal with it.
//   Windows - WXCOLORREF
//   GTK     - int
//   X11     - long
//   Mac     - (WXCOLORREF&)
// EWXWEXPORT(WXCOLORREF, wxColour_GetPixel)(void* _obj)
// {
// 	return ((wxColour*)_obj)->GetPixel();
// }

EWXWEXPORT(void, wxColour_Copy)(void* _obj, void* _other)
{
	(*((wxColour*)_obj)) = (*((wxColour*)_other));
}

EWXWEXPORT(void, wxColour_SetByName)(void* _obj, void* _name)
{
	(*((wxColour*)_obj)) = (wxChar*)_name;
}

EWXWEXPORT(int, wxColour_ValidName)(wxChar* _name)
{
#if (wxVERSION_NUMBER < 2600)
  return (wxTheColourDatabase->FindColour (wxString(_name))) != NULL;
#else
  return wxTheColourDatabase->Find(wxString(_name)).Ok();
#endif
}

}
