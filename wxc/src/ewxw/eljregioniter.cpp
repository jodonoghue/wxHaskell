#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxRegionIterator_Create)()
{
	return (void*) new wxRegionIterator();
}

EWXWEXPORT(void*, wxRegionIterator_CreateFromRegion)(void* region)
{
	return (void*) new wxRegionIterator(*((wxRegion*)region));
}

EWXWEXPORT(void, wxRegionIterator_Delete)(void* _obj)
{
	delete (wxRegionIterator*)_obj;
}

EWXWEXPORT(void, wxRegionIterator_Reset)(void* _obj)
{
	((wxRegionIterator*)_obj)->Reset();
}
	
EWXWEXPORT(void, wxRegionIterator_ResetToRegion)(void* _obj, void* region)
{
	((wxRegionIterator*)_obj)->Reset(*((wxRegion*)region));
}
	
EWXWEXPORT(int, wxRegionIterator_HaveRects)(void* _obj)
{
	return (int)((wxRegionIterator*)_obj)->HaveRects();
}

EWXWEXPORT(void, wxRegionIterator_Next)(void* _obj)
{
	(*((wxRegionIterator*)_obj))++;
}
	
EWXWEXPORT(int, wxRegionIterator_GetX)(void* _obj)
{
	return ((wxRegionIterator*)_obj)->GetX();
}
	
EWXWEXPORT(int, wxRegionIterator_GetY)(void* _obj)
{
	return ((wxRegionIterator*)_obj)->GetY();
}
	
EWXWEXPORT(int, wxRegionIterator_GetWidth)(void* _obj)
{
	return ((wxRegionIterator*)_obj)->GetWidth();
}
	
EWXWEXPORT(int, wxRegionIterator_GetHeight)(void* _obj)
{
	return ((wxRegionIterator*)_obj)->GetHeight();
}
	
}
