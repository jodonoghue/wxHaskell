#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, Cursor_CreateFromStock)(int _id)
{
	return (void*) new wxCursor(_id);
}

}
