#include "wrapper.h"

extern "C"
{
	
EWXWEXPORT(void*, wxRegion_CreateDefault) ()
{
	return (void*) new wxRegion();
}

EWXWEXPORT(void*, wxRegion_CreateFromRect) (int x, int y, int w, int h)
{
	return (void*) new wxRegion((wxCoord)x, (wxCoord)y, (wxCoord)w, (wxCoord)h);
}

EWXWEXPORT(void, wxRegion_Delete) (void* _obj)
{
	delete (wxRegion*)_obj;
}

EWXWEXPORT(void, wxRegion_Assign)(void* _obj, void* region)
{
	*((wxRegion*)_obj) = *((wxRegion*)region);
}

EWXWEXPORT(void, wxRegion_Clear)(void* _obj)
{
	((wxRegion*)_obj)->Clear();
}
	
EWXWEXPORT(int, wxRegion_UnionRect)(void* _obj, int x, int y, int width, int height)
{
	return (int)((wxRegion*)_obj)->Union((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(int, wxRegion_UnionRegion)(void* _obj, void* region)
{
	return (int)((wxRegion*)_obj)->Union(*((wxRegion*)region));
}
	
EWXWEXPORT(int, wxRegion_IntersectRect)(void* _obj, int x, int y, int width, int height)
{
	return (int)((wxRegion*)_obj)->Intersect((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(int, wxRegion_IntersectRegion)(void* _obj, void* region)
{
	return (int)((wxRegion*)_obj)->Intersect(*((wxRegion*)region));
}
	
EWXWEXPORT(int, wxRegion_SubtractRect)(void* _obj, int x, int y, int width, int height)
{
	return (int)((wxRegion*)_obj)->Subtract((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(int, wxRegion_SubtractRegion)(void* _obj, void* region)
{
	return (int)((wxRegion*)_obj)->Subtract(*((wxRegion*)region));
}
	
EWXWEXPORT(int, wxRegion_XorRect)(void* _obj, int x, int y, int width, int height)
{
	return (int)((wxRegion*)_obj)->Xor((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(int, wxRegion_XorRegion)(void* _obj, void* region)
{
	return (int)((wxRegion*)_obj)->Xor(*((wxRegion*)region));
}
	
EWXWEXPORT(void, wxRegion_GetBox)(void* _obj, void* x, void* y, void* w, void* h)
{
	((wxRegion*)_obj)->GetBox(*((wxCoord*)x), *((wxCoord*)y), *((wxCoord*)w), *((wxCoord*)h));
}
	
EWXWEXPORT(int, wxRegion_Empty)(void* _obj)
{
	return (int)((wxRegion*)_obj)->Empty();
}
	
EWXWEXPORT(int, wxRegion_ContainsPoint)(void* _obj, int x, int y)
{
	return (int)((wxRegion*)_obj)->Contains((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(int, wxRegion_ContainsRect)(void* _obj, int x, int y, int width, int height)
{
	return (int)((wxRegion*)_obj)->Contains((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
}
