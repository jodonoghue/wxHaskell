#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxIcon_CreateDefault)()
{
	return (void*) new wxIcon();
}

EWXWEXPORT(void, wxIcon_Delete)(void* _obj)
{
	delete (wxIcon*)_obj;
}

EWXWEXPORT(void*, wxIcon_FromRaw)(void* data, int width, int height)
{
#ifdef __WIN32__
	return (void*) new wxIcon((const char*) data, wxBITMAP_TYPE_ICO, width, height);
#else
	return (void*) new wxIcon((const char*) data, wxBITMAP_TYPE_ANY, width, height);
#endif
}

EWXWEXPORT(void*, wxIcon_FromXPM)(void* data)
{
	return (void*) new wxIcon((char**) data);
}

EWXWEXPORT(void*, wxIcon_CreateLoad) (void* name, long type, int width, int height)
{
	return (void*) new wxIcon((char*)name, (wxBitmapType)type, width, height);
}

EWXWEXPORT(int, wxIcon_Load)(void* _obj, void* name, long type, int width, int height)
{
#ifdef __WIN32__
	return (int)((wxIcon*)_obj)->LoadFile((char*)name, (wxBitmapType)type, width, height);
#else
	return (int)((wxIcon*)_obj)->LoadFile((char*)name, (wxBitmapType)type);
#endif
}

EWXWEXPORT(void, wxIcon_CopyFromBitmap)(void* _obj, void* bmp)
{
#ifdef __WIN32__
	((wxIcon*)_obj)->CopyFromBitmap(*((wxBitmap*)bmp));
#endif
}

EWXWEXPORT(int, wxIcon_Ok)(void* _obj)
{
	return (int)((wxIcon*)_obj)->Ok();
}

EWXWEXPORT(int, wxIcon_GetDepth)(void* _obj)
{
	return (int)((wxIcon*)_obj)->GetDepth();
}

EWXWEXPORT(int, wxIcon_GetWidth)(void* _obj)
{
	return (int)((wxIcon*)_obj)->GetWidth();
}

EWXWEXPORT(int, wxIcon_GetHeight)(void* _obj)
{
	return (int)((wxIcon*)_obj)->GetHeight();
}

EWXWEXPORT(void, wxIcon_Assign)(void* _obj, void* other)
{
	*((wxIcon*)_obj) = *((wxIcon*)other);
}

EWXWEXPORT(int, wxIcon_IsEqual)(void* _obj, void* other)
{
	return (int)(*((wxIcon*)_obj) == *((wxIcon*)other));
}

}
