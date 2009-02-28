#include "wrapper.h"

extern "C"
{

EWXWEXPORT(wxCursor*,Cursor_CreateFromStock)(int _id)
{
	return  new wxCursor(_id);
}

EWXWEXPORT(wxCursor*,Cursor_CreateFromImage)(wxImage* image)
{
	return  new wxCursor(*image);
}

EWXWEXPORT(wxCursor*,Cursor_CreateLoad)(wxString* name,long type,int width,int height)
{
#ifdef __WXGTK__
// See http://thread.gmane.org/gmane.comp.lib.wxwidgets.general/45999
	return NULL;
#else
	return new wxCursor(*name, type, width, height);
#endif
}

}
