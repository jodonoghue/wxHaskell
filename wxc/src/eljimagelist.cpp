#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxImageList_Create)(int width, int height, int mask, int initialCount)
{
	return (void*) new wxImageList(width, height, mask != 0, initialCount);
}
	
EWXWEXPORT(void, wxImageList_Delete)(void* _obj)
{
	delete (wxImageList*)_obj;
}

EWXWEXPORT(int, wxImageList_GetImageCount)(void* _obj)
{
	return ((wxImageList*)_obj)->GetImageCount();
}
	
EWXWEXPORT(void, wxImageList_GetSize)(void* _obj, int index, int* width, int* height)
{
	bool success = ((wxImageList*)_obj)->GetSize(index, *((int*)width), *((int*)height));
        if (!success) {
          *width = -1;
          *height = -1;
        };
}
	
EWXWEXPORT(int, wxImageList_AddBitmap)(void* _obj, void* bitmap, void* mask)
{
	return ((wxImageList*)_obj)->Add(*((wxBitmap*)bitmap), *((wxBitmap*)mask));
}
	
EWXWEXPORT(int, wxImageList_AddMasked)(void* _obj, void* bitmap, void* maskColour)
{
	return ((wxImageList*)_obj)->Add(*((wxBitmap*)bitmap), *((wxColour*)maskColour));
}
	
EWXWEXPORT(int, wxImageList_AddIcon)(void* _obj, void* icon)
{
	return ((wxImageList*)_obj)->Add(*((wxIcon*)icon));
}
	
EWXWEXPORT(int, wxImageList_Replace)(void* _obj, int index, void* bitmap, void* mask)
{
#ifdef __WIN32__
	return (int)((wxImageList*)_obj)->Replace(index, *((wxBitmap*)bitmap), *((wxBitmap*)mask));
#else
	return (int)((wxImageList*)_obj)->Replace(index, *((wxBitmap*)bitmap));
#endif
}
	
EWXWEXPORT(int, wxImageList_ReplaceIcon)(void* _obj, int index, void* icon)
{
	return (int)((wxImageList*)_obj)->Replace(index, *((wxIcon*)icon));
}
	
EWXWEXPORT(int, wxImageList_Remove)(void* _obj, int index)
{
	return (int)((wxImageList*)_obj)->Remove(index);
}
	
EWXWEXPORT(int, wxImageList_RemoveAll)(void* _obj)
{
	return (int)((wxImageList*)_obj)->RemoveAll();
}
	
EWXWEXPORT(int, wxImageList_Draw)(void* _obj, int index, void* dc, int x, int y, int flags, int solidBackground)
{
	return (int)((wxImageList*)_obj)->Draw(index, *((wxDC*)dc), x, y, flags, solidBackground != 0);
}
	
}
