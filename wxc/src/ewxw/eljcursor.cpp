#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, Cursor_CreateFromStock)(int _id)
{
	return (void*) new wxCursor(_id);
}

EWXWEXPORT(void*, Cursor_CreateFromImage)(wxImage* image)
{
	return (void*) new wxCursor(*image);
}

EWXWEXPORT(void*, Cursor_CreateLoad) (const wxString* name, long type, int width, int height)
{
	return (void*) new wxCursor(*name, type, width, height);
}

}
