#include "wrapper.h"

wxDragResult ELJTextDropTarget::OnData(wxCoord x, wxCoord y, wxDragResult def)
{
	if (on_data_func)
		return (wxDragResult) on_data_func(obj, (long)x, (long)y, (int) def);
	else
		return wxTextDropTarget::OnData(x, y, def);
}

bool ELJTextDropTarget::OnDrop(wxCoord x, wxCoord y)
{
	if (on_drop_func)
		return (bool) on_drop_func(obj, (long)x, (long)y);
	else
		return wxTextDropTarget::OnDrop(x, y);
}

wxDragResult ELJTextDropTarget::OnEnter(wxCoord x, wxCoord y, wxDragResult def)
{
	if (on_enter_func)
		return (wxDragResult) on_enter_func(obj, (long)x, (long)y, (int) def);
	else
		return wxTextDropTarget::OnEnter(x, y, def);
}

wxDragResult ELJTextDropTarget::OnDragOver(wxCoord x, wxCoord y, wxDragResult def)
{
	if (on_drag_func)
		return (wxDragResult) on_drag_func(obj, (long)x, (long)y, (int) def);
	else
		return wxTextDropTarget::OnDragOver(x, y, def);
}

void ELJTextDropTarget::OnLeave()
{
	if (on_leave_func)
		on_leave_func(obj);
	else
		wxTextDropTarget::OnLeave();
}

wxDragResult ELJFileDropTarget::OnData(wxCoord x, wxCoord y, wxDragResult def)
{
	if (on_data_func)
		return (wxDragResult) on_data_func(obj, (long)x, (long)y, (int) def);
	else
		return wxFileDropTarget::OnData(x, y, def);
}

bool ELJFileDropTarget::OnDrop(wxCoord x, wxCoord y)
{
	if (on_drop_func)
		return (bool) on_drop_func(obj, (long)x, (long)y);
	else
		return wxFileDropTarget::OnDrop(x, y);
}

wxDragResult ELJFileDropTarget::OnEnter(wxCoord x, wxCoord y, wxDragResult def)
{
	if (on_enter_func)
		return (wxDragResult) on_enter_func(obj, (long)x, (long)y, (int) def);
	else
		return wxFileDropTarget::OnEnter(x, y, def);
}

wxDragResult ELJFileDropTarget::OnDragOver(wxCoord x, wxCoord y, wxDragResult def)
{
	if (on_drag_func)
		return (wxDragResult) on_drag_func(obj, (long)x, (long)y, (int) def);
	else
		return wxFileDropTarget::OnDragOver(x, y, def);
}

void ELJFileDropTarget::OnLeave()
{
	if (on_leave_func)
		on_leave_func(obj);
	else
		wxFileDropTarget::OnLeave();
}

wxDragResult ELJDropTarget::OnData(wxCoord x, wxCoord y, wxDragResult def)
{
	if (on_data_func)
		return (wxDragResult) on_data_func(obj, (long)x, (long)y, (int) def);
	else
	{
		GetData();
		return def;
	}
}

bool ELJDropTarget::OnDrop(wxCoord x, wxCoord y)
{
	if (on_drop_func)
		return (bool) on_drop_func(obj, (long)x, (long)y);
	else
		return wxDropTarget::OnDrop(x, y);
}

wxDragResult ELJDropTarget::OnEnter(wxCoord x, wxCoord y, wxDragResult def)
{
	if (on_enter_func)
		return (wxDragResult) on_enter_func(obj, (long)x, (long)y, (int) def);
	else
		return wxDropTarget::OnEnter(x, y, def);
}

wxDragResult ELJDropTarget::OnDragOver(wxCoord x, wxCoord y, wxDragResult def)
{
	if (on_drag_func)
		return (wxDragResult) on_drag_func(obj, (long)x, (long)y, (int) def);
	else
		return wxDropTarget::OnDragOver(x, y, def);
}

void ELJDropTarget::OnLeave()
{
	if (on_leave_func)
		on_leave_func(obj);
	else
		wxDropTarget::OnLeave();
}

bool ELJFileDropTarget::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames)
{
	bool result = false;
	const char** arr = (const char**)malloc (sizeof(char*) * filenames.GetCount());
	
	for (unsigned int i = 0; i < filenames.GetCount(); i++)
		arr[i] = filenames.Item(i).c_str();
	
	result = func(obj, (long)x, (long)y, (void*)arr, (int)filenames.GetCount()) != 0;
	free(arr);
	
	return result;
}

bool ELJTextDropTarget::OnDropText(wxCoord x, wxCoord y, const wxString& text)
{
	return func(obj, (long)x, (long)y, (void*)text.c_str()) != 0;
}

extern "C"
{

EWXWEXPORT(void*, ELJFileDropTarget_Create)(void* _obj, void* _func)
{
	return (void*) new ELJFileDropTarget(_obj, (FileDropFunc)_func);
}

EWXWEXPORT(void, ELJFileDropTarget_Delete)(void* _obj)
{
	delete (ELJFileDropTarget*)_obj;
}

EWXWEXPORT(void*, ELJTextDropTarget_Create)(void* _obj, void* _func)
{
	return (void*) new ELJTextDropTarget(_obj, (TextDropFunc)_func);
}

EWXWEXPORT(void, ELJTextDropTarget_Delete)(void* _obj)
{
	delete (ELJTextDropTarget*)_obj;
}

EWXWEXPORT(void*, TextDataObject_Create)(void* _txt)
{
	return (void*) new wxTextDataObject((char*)_txt);
}

EWXWEXPORT(void, TextDataObject_Delete)(void* _obj)
{
	delete (wxTextDataObject*)_obj;
}

EWXWEXPORT(void*, FileDataObject_Create)(void* _lst, int _cnt)
{
	wxFileDataObject* result = new wxFileDataObject();
	if (_cnt)
	{
		for (int i = 0; i < _cnt; i++)
			result->AddFile(((char**)_lst)[i]);
	}
	return (void*) result;
}

EWXWEXPORT(void, FileDataObject_Delete)(void* _obj)
{
	delete (wxFileDataObject*)_obj;
}

EWXWEXPORT(void, FileDataObject_AddFile)(void* _obj, void* _fle)
{
	((wxFileDataObject*)_obj)->AddFile((char*)_fle);
}

EWXWEXPORT(int, FileDataObject_GetFilenames)(void* _obj, void* _lst)
{
	wxArrayString arr = ((wxFileDataObject*)_obj)->GetFilenames();
	if (_lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((const char**)_lst)[i] = strdup (arr.Item(i).c_str());
	}
	return arr.GetCount();
}


EWXWEXPORT(void*, BitmapDataObject_Create)(void* _bmp)
{
	return (void*) new wxBitmapDataObject(*((wxBitmap*)_bmp));
}

EWXWEXPORT(void*, BitmapDataObject_CreateEmpty)()
{
	return (void*) new wxBitmapDataObject();
}

EWXWEXPORT(void, BitmapDataObject_Delete)(void* _obj)
{
	delete (wxBitmapDataObject*)_obj;
}

EWXWEXPORT(void, BitmapDataObject_SetBitmap)(void* _obj, void* _bmp)
{
	((wxBitmapDataObject*)_obj)->SetBitmap (*((wxBitmap*)_bmp));
}

EWXWEXPORT(void, BitmapDataObject_GetBitmap)(void* _obj, void* _bmp)
{
	*((wxBitmap*)_bmp) = ((wxBitmapDataObject*)_obj)->GetBitmap ();
}


EWXWEXPORT(void*, DropSource_Create)(void* data, void* win, void* copy, void* move, void* none)
{
#if (wxCHECK_VERSION(2,5,0) && defined(__WXMAC__)) || defined(__WIN32__)
	return (void*) new wxDropSource(*((wxDataObject*)data), (wxWindow*)win, *((wxCursor*)copy), *((wxCursor*)move), *((wxCursor*)none));
#else
	return (void*) new wxDropSource(*((wxDataObject*)data), (wxWindow*)win, *((wxIcon*)copy), *((wxIcon*)move), *((wxIcon*)none));
#endif
}

EWXWEXPORT(void, DropSource_Delete)(void* _obj)
{
	delete (wxDropSource*)_obj;
}

EWXWEXPORT(int, DropSource_DoDragDrop)(void* _obj, int _move)
{
	return (int)((wxDropSource*)_obj)->DoDragDrop(_move != 0);
}

EWXWEXPORT(void*, ELJDropTarget_Create)(void* _obj)
{
	return (void*) new ELJDropTarget(_obj);
}

EWXWEXPORT(void, ELJDropTarget_Delete)(void* _obj)
{
	delete (ELJDropTarget*)_obj;
}

EWXWEXPORT(void, ELJFileDropTarget_SetOnData)(void* _obj, void* _func)
{
	((ELJFileDropTarget*)_obj)->SetOnData((DragThreeFunc)_func);
}

EWXWEXPORT(void, ELJFileDropTarget_SetOnDrop)(void* _obj, void* _func)
{
	((ELJFileDropTarget*)_obj)->SetOnDrop((DragTwoFunc)_func);
}

EWXWEXPORT(void, ELJFileDropTarget_SetOnEnter)(void* _obj, void* _func)
{
	((ELJFileDropTarget*)_obj)->SetOnEnter((DragThreeFunc)_func);
}

EWXWEXPORT(void, ELJFileDropTarget_SetOnDragOver)(void* _obj, void* _func)
{
	((ELJFileDropTarget*)_obj)->SetOnDragOver((DragThreeFunc)_func);
}

EWXWEXPORT(void, ELJFileDropTarget_SetOnLeave)(void* _obj, void* _func)
{
	((ELJFileDropTarget*)_obj)->SetOnLeave((DragZeroFunc)_func);
}

EWXWEXPORT(void, ELJTextDropTarget_SetOnData)(void* _obj, void* _func)
{
	((ELJTextDropTarget*)_obj)->SetOnData((DragThreeFunc)_func);
}

EWXWEXPORT(void, ELJTextDropTarget_SetOnDrop)(void* _obj, void* _func)
{
	((ELJTextDropTarget*)_obj)->SetOnDrop((DragTwoFunc)_func);
}

EWXWEXPORT(void, ELJTextDropTarget_SetOnEnter)(void* _obj, void* _func)
{
	((ELJTextDropTarget*)_obj)->SetOnEnter((DragThreeFunc)_func);
}

EWXWEXPORT(void, ELJTextDropTarget_SetOnDragOver)(void* _obj, void* _func)
{
	((ELJTextDropTarget*)_obj)->SetOnDragOver((DragThreeFunc)_func);
}

EWXWEXPORT(void, ELJTextDropTarget_SetOnLeave)(void* _obj, void* _func)
{
	((ELJTextDropTarget*)_obj)->SetOnLeave((DragZeroFunc)_func);
}

EWXWEXPORT(void, ELJDropTarget_SetOnData)(void* _obj, void* _func)
{
	((ELJDropTarget*)_obj)->SetOnData((DragThreeFunc)_func);
}

EWXWEXPORT(void, ELJDropTarget_SetOnDrop)(void* _obj, void* _func)
{
	((ELJDropTarget*)_obj)->SetOnDrop((DragTwoFunc)_func);
}

EWXWEXPORT(void, ELJDropTarget_SetOnEnter)(void* _obj, void* _func)
{
	((ELJDropTarget*)_obj)->SetOnEnter((DragThreeFunc)_func);
}

EWXWEXPORT(void, ELJDropTarget_SetOnDragOver)(void* _obj, void* _func)
{
	((ELJDropTarget*)_obj)->SetOnDragOver((DragThreeFunc)_func);
}

EWXWEXPORT(void, ELJDropTarget_SetOnLeave)(void* _obj, void* _func)
{
	((ELJDropTarget*)_obj)->SetOnLeave((DragZeroFunc)_func);
}

EWXWEXPORT(void, wxDropTarget_GetData)(void* _obj)
{
	((wxDropTarget*)_obj)->GetData();
}

EWXWEXPORT(void, wxDropTarget_SetDataObject)(void* _obj, void* _dat)
{
	((wxDropTarget*)_obj)->SetDataObject((wxDataObject*)_dat);
}

EWXWEXPORT(void*, ELJDragDataObject_Create)(void* _obj, void* _fmt, void* _func1, void* _func2, void* _func3)
{
	return (void*) new ELJDragDataObject(_obj, (char*)_fmt, (DataGetDataSize)_func1, (DataGetDataHere)_func2, (DataSetData)_func3);
}

EWXWEXPORT(void, ELJDragDataObject_Delete)(void* _obj)
{
	delete (ELJDragDataObject*)_obj;
}

EWXWEXPORT(void*, wxDataObjectComposite_Create)()
{
	return (void*) new wxDataObjectComposite();
}

EWXWEXPORT(void, wxDataObjectComposite_Delete)(void* _obj)
{
	delete (wxDataObjectComposite*)_obj;
}

EWXWEXPORT(void, wxDataObjectComposite_Add)(void* _obj, void* _dat, int _preferred)
{
	((wxDataObjectComposite*)_obj)->Add((wxDataObjectSimple*)_dat, _preferred != 0);
}

}
