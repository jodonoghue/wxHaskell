#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxFontDialog_Create) (void* _prt, void* fnt)
{
	return (void*) new wxFontDialog ((wxWindow*)_prt, (wxFontData*) fnt);
}

EWXWEXPORT(void, wxFontDialog_GetFontData)(void* _obj, void* _ref)
{
	*((wxFontData*)_ref) = ((wxFontDialog*)_obj)->GetFontData();
}

}
