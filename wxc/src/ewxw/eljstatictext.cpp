#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxStaticText_Create) (void* _prt, int _id, char* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxStaticText ((wxWindow*)_prt, _id, _txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

}
