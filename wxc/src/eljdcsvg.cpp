#include "wrapper.h"

#ifdef wxUSE_SVG
# include "wx/svg/dcsvg.h"
#endif

extern "C"
{

EWXWEXPORT(void*, wxSVGFileDC_Create)(wxChar* a_filename)
{
#ifdef wxUSE_SVG
	return (void*) new wxSVGFileDC(wxString(a_filename));
#else
	return NULL;
#endif
}

EWXWEXPORT(void*, wxSVGFileDC_CreateWithSize)(wxChar* a_filename, int a_width, int a_height)
{
#ifdef wxUSE_SVG
	return (void*) new wxSVGFileDC(wxString(a_filename), a_width, a_height);
#else
	return NULL;
#endif
}

EWXWEXPORT(void*, wxSVGFileDC_CreateWithSizeAndResolution)(wxChar* a_filename, int a_width, int a_height, float a_dpi)
{
#ifdef wxUSE_SVG
	return (void*) new wxSVGFileDC(wxString(a_filename), a_width, a_height, a_dpi);
#else
	return NULL;
#endif
}

EWXWEXPORT(void, wxSVGFileDC_Delete) (void* _obj)
{
#ifdef wxUSE_SVG
	delete (wxSVGFileDC*)_obj;
#endif
}

}
