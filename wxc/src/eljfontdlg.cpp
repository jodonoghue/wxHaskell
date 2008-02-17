#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxFontDialog_Create) (void* _prt, void* fnt)
{
#ifdef wxMAC_USE_EXPERIMENTAL_FONTDIALOG
	return (void*) new wxFontDialog ((wxWindow*)_prt, (wxFontData&) fnt);
#else
	return (void*) new wxFontDialog ((wxWindow*)_prt, (wxFontData*) fnt);
#endif
}

EWXWEXPORT(void, wxFontDialog_GetFontData)(void* _obj, void* _ref)
{
	*((wxFontData*)_ref) = ((wxFontDialog*)_obj)->GetFontData();
}

}
