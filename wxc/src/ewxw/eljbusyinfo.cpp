#include "wrapper.h"
#include "wx/busyinfo.h"

extern "C"
{

EWXWEXPORT(void*, wxBusyInfo_Create) (void* _txt)
{
	return (void*) new wxBusyInfo ((wxChar*)_txt);
}

EWXWEXPORT(void, wxBusyInfo_Delete) (void* _obj)
{
	delete (wxBusyInfo*)_obj;
}

EWXWEXPORT(void*, wxBusyCursor_Create) ()
{
	return (void*) new wxBusyCursor ();
}

EWXWEXPORT(void*, wxBusyCursor_CreateWithCursor) (void* _cur)
{
	return (void*) new wxBusyCursor ((wxCursor*)_cur);
}

EWXWEXPORT(void, wxBusyCursor_Delete) (void* _obj)
{
	delete (wxBusyCursor*)_obj;
}

}
