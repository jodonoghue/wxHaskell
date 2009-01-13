#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxBitmap_Create)(void* _data,int _type,int _width,int _height,int _depth)
{
#ifdef __WIN32__
	return (void*) new wxBitmap(_data, _type, _width, _height, _depth);
#else
	return (void*) new wxBitmap((const char*)_data, _width, _height, _depth);
#endif
}

EWXWEXPORT(void*,wxBitmap_CreateFromXPM)(void* _data)
{
	return (void*) new wxBitmap((const char**)_data);
}

EWXWEXPORT(void*,wxBitmap_CreateEmpty)(int _width,int _height,int _depth)
{
	return (void*) new wxBitmap(_width, _height, _depth);
}

EWXWEXPORT(void*,wxBitmap_CreateLoad)(wxString* name,int type)
{
#if wxVERSION_NUMBER >= 2400
	return (void*) new wxBitmap(*name, (wxBitmapType)type);
#else
	return (void*) new wxBitmap(*name, (long)type);
#endif
}

EWXWEXPORT(void*,wxBitmap_CreateDefault)()
{
	return (void*) new wxBitmap();
}

EWXWEXPORT(void,wxBitmap_Delete)(wxBitmap* _obj)
{
	delete _obj;
}

EWXWEXPORT(void,wxBitmap_GetSubBitmap)(wxBitmap* _obj,int x,int y,int w,int h,wxBitmap* bitmap)
{
	*bitmap = _obj->GetSubBitmap(wxRect(x, y, w, h));
}

EWXWEXPORT(bool,wxBitmap_LoadFile)(wxBitmap* _obj,wxString* name,int type)
{
#if wxVERSION_NUMBER >= 2400
	return _obj->LoadFile(*name, (wxBitmapType)type);
#else
	return _obj->LoadFile(*name, (long)type);
#endif
}

EWXWEXPORT(bool,wxBitmap_SaveFile)(wxBitmap* _obj,wxString* name,int type,wxPalette* cmap)
{
#if wxVERSION_NUMBER >= 2400
	return _obj->SaveFile(*name, (wxBitmapType)type,  cmap);
#else
	return _obj->SaveFile(*name, type,  cmap);
#endif
}

EWXWEXPORT(void*,wxBitmap_GetMask)(wxBitmap* _obj)
{
	return (void*)_obj->GetMask();
}

EWXWEXPORT(void,wxBitmap_SetMask)(wxBitmap* _obj,void* mask)
{
	_obj->SetMask((wxMask*) mask);
}

/**/
EWXWEXPORT(void,wxBitmap_AddHandler)(void* handler)
{
#ifdef __WIN32__
	wxBitmap::AddHandler((wxGDIImageHandler*) handler);
#endif
}

EWXWEXPORT(void,wxBitmap_InsertHandler)(void* handler)
{
#ifdef __WIN32__
	wxBitmap::InsertHandler((wxGDIImageHandler*) handler);
#endif
}

EWXWEXPORT(bool,wxBitmap_RemoveHandler)(wxString* name)
{
#ifdef __WIN32__
	return wxBitmap::RemoveHandler(*name);
#else
	return false;
#endif
}

EWXWEXPORT(void*,wxBitmap_FindHandlerByName)(wxString* name)
{
#ifdef __WIN32__
	return (void*)wxBitmap::FindHandler(*name);
#else
	return NULL;
#endif
}

EWXWEXPORT(void*,wxBitmap_FindHandlerByExtension)(wxString* extension,int type)
{
#ifdef __WIN32__
	return (void*)wxBitmap::FindHandler(*extension, (long)type);
#else
	return NULL;
#endif
}

EWXWEXPORT(void*,wxBitmap_FindHandlerByType)(int type)
{
#ifdef __WIN32__
	return (void*)wxBitmap::FindHandler((long)type);
#else
	return NULL;
#endif
}

EWXWEXPORT(void,wxBitmap_InitStandardHandlers)()
{
#ifdef __WIN32__
	wxBitmap::InitStandardHandlers();
#endif
}

EWXWEXPORT(void,wxBitmap_CleanUpHandlers)()
{
#ifdef __WIN32__
	wxBitmap::CleanUpHandlers();
#endif
}

/**/
EWXWEXPORT(bool,wxBitmap_IsOk)(wxBitmap* _obj)
{
	return _obj->IsOk();
}

EWXWEXPORT(int,wxBitmap_GetWidth)(wxBitmap* _obj)
{
	return _obj->GetWidth();
}

EWXWEXPORT(int,wxBitmap_GetHeight)(wxBitmap* _obj)
{
	return _obj->GetHeight();
}

EWXWEXPORT(int,wxBitmap_GetDepth)(wxBitmap* _obj)
{
	return _obj->GetDepth();
}

EWXWEXPORT(void,wxBitmap_SetWidth)(wxBitmap* _obj,int w)
{
	_obj->SetWidth(w);
}

EWXWEXPORT(void,wxBitmap_SetHeight)(wxBitmap* _obj,int h)
{
	_obj->SetHeight(h);
}

EWXWEXPORT(void,wxBitmap_SetDepth)(wxBitmap* _obj,int d)
{
	_obj->SetDepth(d);
}

EWXWEXPORT(void*,wxStaticBitmap_Create)(wxWindow* _prt,int _id,void* bitmap, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxStaticBitmap (_prt, _id, *((wxBitmap*)bitmap), wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void,wxStaticBitmap_SetIcon)(wxStaticBitmap* _obj,void* icon)
{
	_obj->SetIcon(*((wxIcon*)icon));
}

EWXWEXPORT(void,wxStaticBitmap_SetBitmap)(wxStaticBitmap* _obj,void* bitmap)
{
	_obj->SetBitmap(*((wxBitmap*)bitmap));
}

EWXWEXPORT(void,wxStaticBitmap_GetIcon)(wxStaticBitmap* _obj,void* _ref)
{
	*((wxIcon*)_ref) = _obj->GetIcon();
}

EWXWEXPORT(void,wxStaticBitmap_GetBitmap)(wxStaticBitmap* _obj,void* _ref)
{
	*((wxBitmap*)_ref) = _obj->GetBitmap();
}

EWXWEXPORT(void,wxStaticBitmap_Delete)(wxStaticBitmap* _obj)
{
	delete _obj;
}

}
