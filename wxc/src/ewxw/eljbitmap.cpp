#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxBitmap_Create)(void* _data, int _type, int _width, int _height, int _depth)
{
#ifdef __WIN32__
	return (void*) new wxBitmap(_data, _type, _width, _height, _depth);
#else
	return (void*) new wxBitmap((const char*)_data, _width, _height, _depth);
#endif
}

EWXWEXPORT(void*, wxBitmap_CreateFromXPM)(void* _data)
{
	return (void*) new wxBitmap((const char**)_data);
}

EWXWEXPORT(void*, wxBitmap_CreateEmpty)(int _width, int _height, int _depth)
{
	return (void*) new wxBitmap(_width, _height, _depth);
}

EWXWEXPORT(void*, wxBitmap_CreateLoad)(void* name, int type)
{
#if wxVERSION_NUMBER >= 2400
	return (void*) new wxBitmap((char*)name, (wxBitmapType)type);
#else
	return (void*) new wxBitmap((char*)name, (long)type);
#endif
}

EWXWEXPORT(void*, wxBitmap_CreateDefault)()
{
	return (void*) new wxBitmap();
}

EWXWEXPORT(void, wxBitmap_Delete)(void* _obj)
{
	delete (wxBitmap*)_obj;
}

EWXWEXPORT(void, wxBitmap_GetSubBitmap)(void* _obj, int x, int y, int w, int h, void* bitmap)
{
	(*(wxBitmap*)bitmap) = ((wxBitmap*)_obj)->GetSubBitmap(wxRect(x, y, w, h));
}
	
EWXWEXPORT(int, wxBitmap_LoadFile)(void* _obj, void* name, int type)
{
#if wxVERSION_NUMBER >= 2400
	return (int)((wxBitmap*)_obj)->LoadFile((char*)name, (wxBitmapType)type);
#else
	return (int)((wxBitmap*)_obj)->LoadFile((char*)name, (long)type);
#endif
}
	
EWXWEXPORT(int, wxBitmap_SaveFile)(void* _obj, void* name, int type, void* cmap)
{
#if wxVERSION_NUMBER >= 2400
	return (int)((wxBitmap*)_obj)->SaveFile((char*) name, (wxBitmapType)type, (wxPalette*) cmap);
#else
	return (int)((wxBitmap*)_obj)->SaveFile((char*) name, type, (wxPalette*) cmap);
#endif
}
	
EWXWEXPORT(void*, wxBitmap_GetMask)(void* _obj)
{
	return (void*)((wxBitmap*)_obj)->GetMask();
}
	
EWXWEXPORT(void, wxBitmap_SetMask)(void* _obj, void* mask)
{
	((wxBitmap*)_obj)->SetMask((wxMask*) mask);
}
	
/**/
EWXWEXPORT(void, wxBitmap_AddHandler)(void* handler)
{
#ifdef __WIN32__
	wxBitmap::AddHandler((wxGDIImageHandler*) handler);
#endif
}
	
EWXWEXPORT(void, wxBitmap_InsertHandler)(void* handler)
{
#ifdef __WIN32__
	wxBitmap::InsertHandler((wxGDIImageHandler*) handler);
#endif
}
	
EWXWEXPORT(int, wxBitmap_RemoveHandler)(void* name)
{
#ifdef __WIN32__
	return (int) wxBitmap::RemoveHandler((char*) name);
#else
	return 0;
#endif
}
	
EWXWEXPORT(void*, wxBitmap_FindHandlerByName)(void* name)
{
#ifdef __WIN32__
	return (void*)wxBitmap::FindHandler((char*) name);
#else
	return NULL;
#endif
}
	
EWXWEXPORT(void*, wxBitmap_FindHandlerByExtension)(void* extension, int type)
{
#ifdef __WIN32__
	return (void*)wxBitmap::FindHandler((char*)extension, (long)type);
#else
	return NULL;
#endif
}
	
EWXWEXPORT(void*, wxBitmap_FindHandlerByType)(int type)
{
#ifdef __WIN32__
	return (void*)wxBitmap::FindHandler((long)type);
#else
	return NULL;
#endif
}
	
EWXWEXPORT(void, wxBitmap_InitStandardHandlers)()
{
#ifdef __WIN32__
	wxBitmap::InitStandardHandlers();
#endif
}
	
EWXWEXPORT(void, wxBitmap_CleanUpHandlers)()
{
#ifdef __WIN32__
	wxBitmap::CleanUpHandlers();
#endif
}
	
/**/
EWXWEXPORT(int, wxBitmap_Ok)(void* _obj)
{
	return (int)((wxBitmap*)_obj)->Ok();
}
	
EWXWEXPORT(int, wxBitmap_GetWidth)(void* _obj)
{
	return ((wxBitmap*)_obj)->GetWidth();
}
	
EWXWEXPORT(int, wxBitmap_GetHeight)(void* _obj)
{
	return ((wxBitmap*)_obj)->GetHeight();
}
	
EWXWEXPORT(int, wxBitmap_GetDepth)(void* _obj)
{
	return ((wxBitmap*)_obj)->GetDepth();
}
	
EWXWEXPORT(void, wxBitmap_SetWidth)(void* _obj, int w)
{
	((wxBitmap*)_obj)->SetWidth(w);
}
	
EWXWEXPORT(void, wxBitmap_SetHeight)(void* _obj, int h)
{
	((wxBitmap*)_obj)->SetHeight(h);
}
	
EWXWEXPORT(void, wxBitmap_SetDepth)(void* _obj, int d)
{
	((wxBitmap*)_obj)->SetDepth(d);
}
	
EWXWEXPORT(void*, wxStaticBitmap_Create) (void* _prt, int _id, void* bitmap, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxStaticBitmap ((wxWindow*)_prt, _id, *((wxBitmap*)bitmap), wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void, wxStaticBitmap_SetIcon)(void* _obj, void* icon)
{
	((wxStaticBitmap*)_obj)->SetIcon(*((wxIcon*)icon));
}
	
EWXWEXPORT(void, wxStaticBitmap_SetBitmap)(void* _obj, void* bitmap)
{
	((wxStaticBitmap*)_obj)->SetBitmap(*((wxBitmap*)bitmap));
}
	
EWXWEXPORT(void, wxStaticBitmap_GetIcon)(void* _obj, void* _ref)
{
	*((wxIcon*)_ref) = ((wxStaticBitmap*)_obj)->GetIcon();
}
	
EWXWEXPORT(void, wxStaticBitmap_GetBitmap)(void* _obj, void* _ref)
{
	*((wxBitmap*)_ref) = ((wxStaticBitmap*)_obj)->GetBitmap();
}
	
EWXWEXPORT(void, wxStaticBitmap_Delete)(void* _obj)
{
	delete (wxStaticBitmap*)_obj;
}

}
