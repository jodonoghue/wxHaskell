#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxClipboard_Create)()
{
	return wxTheClipboard;
}

EWXWEXPORT(void, wxClipboard_Delete)(void* _obj)
{
	// delete (wxClipboard*)_obj;
}

EWXWEXPORT(int, wxClipboard_Open)(void* _obj)
{
	return (int)((wxClipboard*)_obj)->Open();
}

EWXWEXPORT(void, wxClipboard_Close)(void* _obj)
{
	((wxClipboard*)_obj)->Close();
}

EWXWEXPORT(int, wxClipboard_IsOpened)(void* _obj)
{
	return (int)((wxClipboard*)_obj)->IsOpened();
}

EWXWEXPORT(int, wxClipboard_SetData)(void* _obj, wxDataObject* data)
{
	return (int)((wxClipboard*)_obj)->SetData(data);
}

EWXWEXPORT(int, wxClipboard_AddData)(void* _obj, wxDataObject* data)
{
	return (int)((wxClipboard*)_obj)->AddData(data);
}

EWXWEXPORT(int, wxClipboard_IsSupported)(void* _obj, wxDataFormat* format)
{
	return (int)((wxClipboard*)_obj)->IsSupported(*format);
}

EWXWEXPORT(int, wxClipboard_GetData)(void* _obj, wxDataObject* data)
{
	return (int)((wxClipboard*)_obj)->GetData(*data);
}

EWXWEXPORT(void, wxClipboard_Clear)(void* _obj)
{
	((wxClipboard*)_obj)->Clear();
}

EWXWEXPORT(int, wxClipboard_Flush)(void* _obj)
{
	return (int)((wxClipboard*)_obj)->Flush();
}

EWXWEXPORT(void, wxClipboard_UsePrimarySelection)(void* _obj, int primary)
{
	((wxClipboard*)_obj)->UsePrimarySelection (primary != 0);
}

}
