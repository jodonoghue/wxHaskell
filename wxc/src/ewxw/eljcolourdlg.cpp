#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxColourDialog_Create) (void* _prt, void* col)
{
	return (void*) new wxColourDialog ((wxWindow*)_prt, (wxColourData*) col);
}

EWXWEXPORT(void, wxColourDialog_GetColourData)(void* _obj, void* col)
{
	*((wxColourData*)col) = ((wxColourDialog*)_obj)->GetColourData();
}

}
