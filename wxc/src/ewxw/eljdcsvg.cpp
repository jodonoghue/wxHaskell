#include "wrapper.h"
#include "wx/svg/dcsvg.h"

extern "C"
{

EWXWEXPORT(void*, wxSVGFileDC_Create)(void* a_filename)
{
	return (void*) new wxSVGFileDC((char*)a_filename);
}

EWXWEXPORT(void*, wxSVGFileDC_CreateWithSize)(void* a_filename, int a_width, int a_height)
{
	return (void*) new wxSVGFileDC((char*)a_filename, a_width, a_height);
}

EWXWEXPORT(void*, wxSVGFileDC_CreateWithSizeAndResolution)(void* a_filename, int a_width, int a_height, float a_dpi)
{
	return (void*) new wxSVGFileDC((char*)a_filename, a_width, a_height, a_dpi);
}

EWXWEXPORT(void, wxSVGFileDC_Delete) (void* _obj)
{
	delete (wxSVGFileDC*)_obj;
}

EWXWEXPORT(void, wxSVGFileDC_ComputeScaleAndOrigin) (void* _obj)
{
	((wxSVGFileDC*)_obj)->ComputeScaleAndOrigin();
}

}
