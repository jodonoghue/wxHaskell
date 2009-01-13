#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,Cursor_CreateFromStock)(int _id)
{
	return (void*) new wxCursor(_id);
}

EWXWEXPORT(void*,Cursor_CreateFromImage)(wxImage* image)
{
	return (void*) new wxCursor(*image);
}

EWXWEXPORT(void*,Cursor_CreateLoad)(wxString* name,long type,int width,int height)
{
#ifdef __WXGTK__
// See http://thread.gmane.org/gmane.comp.lib.wxwidgets.general/45999
	return NULL;
#else
	return (void*) new wxCursor(*name, type, width, height);
#endif
}

}
