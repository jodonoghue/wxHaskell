#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxSizerItem_Create)(int width, int height, int option, int flag, int border, void* userData)
{
	return (void*) new wxSizerItem(width, height, option, flag, border, new ELJDataObject(userData));
}
	
EWXWEXPORT(void*, wxSizerItem_CreateInWindow)(void* window, int option, int flag, int border, void* userData )
{
	return (void*) new wxSizerItem((wxWindow*)window, option, flag, border, new ELJDataObject(userData));
}
	
EWXWEXPORT(void*, wxSizerItem_CreateInSizer)(void* sizer, int option, int flag, int border, void* userData )
{
	return (void*) new wxSizerItem((wxSizer*) sizer, option, flag, border, new ELJDataObject(userData));
}
	
EWXWEXPORT(void, wxSizerItem_GetSize)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxSizerItem*)_obj)->GetSize();
	(*(int*)_h) = res.GetHeight();
	(*(int*)_w) = res.GetWidth();
}
	
EWXWEXPORT(void, wxSizerItem_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxSizerItem*)_obj)->CalcMin();
	(*(int*)_h) = res.GetHeight();
	(*(int*)_w) = res.GetWidth();
}
	
EWXWEXPORT(void, wxSizerItem_SetDimension)(void* _obj,  int _x, int _y, int _w, int _h)
{
	((wxSizerItem*)_obj)->SetDimension(wxPoint(_x, _y), wxSize(_w, _h));
}
	
EWXWEXPORT(void, wxSizerItem_GetMinSize)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxSizerItem*)_obj)->GetMinSize();
	(*(int*)_h) = res.GetHeight();
	(*(int*)_w) = res.GetWidth();
}
	
EWXWEXPORT(void, wxSizerItem_SetRatio)(void* _obj,  int width, int height)
{
	((wxSizerItem*)_obj)->SetRatio(width, height);
}
	
EWXWEXPORT(void, wxSizerItem_SetFloatRatio)(void* _obj,  float ratio)
{
	((wxSizerItem*)_obj)->SetRatio(ratio);
}
	
EWXWEXPORT(float, wxSizerItem_GetRatio)(void* _obj)
{
	return ((wxSizerItem*)_obj)->GetRatio();
}
	
EWXWEXPORT(int, wxSizerItem_IsWindow)(void* _obj)
{
	return (int)((wxSizerItem*)_obj)->IsWindow();
}
	
EWXWEXPORT(int, wxSizerItem_IsSizer)(void* _obj)
{
	return (int)((wxSizerItem*)_obj)->IsSizer();
}
	
EWXWEXPORT(int, wxSizerItem_IsSpacer)(void* _obj)
{
	return (int)((wxSizerItem*)_obj)->IsSpacer();
}
	
EWXWEXPORT(void, wxSizerItem_SetInitSize)(void* _obj, int x, int y)
{
	((wxSizerItem*)_obj)->SetInitSize(x, y);
}
	
EWXWEXPORT(void, wxSizerItem_SetOption)(void* _obj, int option)
{
	((wxSizerItem*)_obj)->SetOption(option);
}
	
EWXWEXPORT(void, wxSizerItem_SetFlag)(void* _obj, int flag)
{
	((wxSizerItem*)_obj)->SetFlag(flag);
}
	
EWXWEXPORT(void, wxSizerItem_SetBorder)(void* _obj, int border)
{
	((wxSizerItem*)_obj)->SetBorder(border);
}
	
EWXWEXPORT(void*, wxSizerItem_GetWindow)(void* _obj)
{
	return (void*)((wxSizerItem*)_obj)->GetWindow();
}
	
EWXWEXPORT(void, wxSizerItem_SetWindow)(void* _obj, void* window)
{
	((wxSizerItem*)_obj)->SetWindow((wxWindow*) window);
}
	
EWXWEXPORT(void*, wxSizerItem_GetSizer)(void* _obj)
{
	return (void*)((wxSizerItem*)_obj)->GetSizer();
}
	
EWXWEXPORT(void, wxSizerItem_SetSizer)(void* _obj, void* sizer)
{
	((wxSizerItem*)_obj)->SetSizer((wxSizer*) sizer);
}
	
EWXWEXPORT(int, wxSizerItem_GetOption)(void* _obj)
{
	return ((wxSizerItem*)_obj)->GetOption();
}
	
EWXWEXPORT(int, wxSizerItem_GetFlag)(void* _obj)
{
	return ((wxSizerItem*)_obj)->GetFlag();
}
	
EWXWEXPORT(int, wxSizerItem_GetBorder)(void* _obj)
{
	return ((wxSizerItem*)_obj)->GetBorder();
}
	
EWXWEXPORT(void*, wxSizerItem_GetUserData)(void* _obj)
{
	return ((ELJDataObject*)((wxSizerItem*)_obj)->GetUserData())->data;
}
	
EWXWEXPORT(void, wxSizerItem_GetPosition)(void* _obj, void* _x, void* _y)
{
	wxPoint res = ((wxSizerItem*)_obj)->GetPosition();
	(*(int*)_x) = res.x;
	(*(int*)_y) = res.y;
}
	
EWXWEXPORT(void, wxSizer_AddWindow)(void* _obj, void* window, int option, int flag, int border, void* userData)
{
	((wxSizer*)_obj)->Add((wxWindow*)window, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void, wxSizer_AddSizer)(void* _obj, void* sizer, int option, int flag, int border, void* userData)
{
	((wxSizer*)_obj)->Add((wxSizer*)sizer, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void, wxSizer_Add)(void* _obj, int width, int height, int option, int flag, int border, void* userData)
{
	((wxSizer*)_obj)->Add(width, height, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void, wxSizer_InsertWindow)(void* _obj, int before, void* window, int option, int flag, int border, void* userData)
{
	((wxSizer*)_obj)->Insert(before, (wxWindow*)window, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void, wxSizer_InsertSizer)(void* _obj, int before, void* sizer, int option, int flag, int border, void* userData)
{
	((wxSizer*)_obj)->Insert(before, (wxSizer*)sizer, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void, wxSizer_Insert)(void* _obj, int before, int width, int height, int option, int flag, int border, void* userData)
{
	((wxSizer*)_obj)->Insert(before, width, height, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void, wxSizer_PrependWindow)(void* _obj, void* window, int option, int flag, int border, void* userData)
{
	((wxSizer*)_obj)->Prepend((wxWindow*)window, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void, wxSizer_PrependSizer)(void* _obj, void* sizer, int option, int flag, int border, void* userData)
{
	((wxSizer*)_obj)->Prepend((wxSizer*)sizer, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void, wxSizer_Prepend)(void* _obj, int width, int height, int option, int flag, int border, void* userData)
{
	((wxSizer*)_obj)->Prepend(width, height, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(int, wxSizer_RemoveWindow)(void* _obj, void* window)
{
	return (int)((wxSizer*)_obj)->Remove((wxWindow*) window);
}
	
EWXWEXPORT(int, wxSizer_RemoveSizer)(void* _obj, void* sizer)
{
	return (int)((wxSizer*)_obj)->Remove((wxSizer*) sizer);
}
	
EWXWEXPORT(int, wxSizer_Remove)(void* _obj, int pos)
{
	return (int)((wxSizer*)_obj)->Remove(pos);
}
	
EWXWEXPORT(void, wxSizer_SetMinSize)(void* _obj, int width, int height)
{
	((wxSizer*)_obj)->SetMinSize(width, height);
}
	
EWXWEXPORT(void, wxSizer_SetItemMinSizeWindow)(void* _obj, void* window, int width, int height)
{
	((wxSizer*)_obj)->SetItemMinSize((wxWindow*) window, width, height);
}
	
EWXWEXPORT(void, wxSizer_SetItemMinSizeSizer)(void* _obj, void* sizer, int width, int height)
{
	((wxSizer*)_obj)->SetItemMinSize((wxSizer*) sizer, width, height);
}
	
EWXWEXPORT(void, wxSizer_SetItemMinSize)(void* _obj, int pos, int width, int height )
{
	((wxSizer*)_obj)->SetItemMinSize(pos, width, height);
}
	
EWXWEXPORT(void, wxSizer_GetSize)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxSizer*)_obj)->GetSize();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void, wxSizer_GetPosition)(void* _obj, void* _x, void* _y)
{
	wxPoint res = ((wxSizer*)_obj)->GetPosition();
	(*(int*)_x) = res.x;
	(*(int*)_y) = res.y;
}
	
EWXWEXPORT(void, wxSizer_GetMinSize)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxSizer*)_obj)->GetMinSize();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void, wxSizer_RecalcSizes)(void* _obj)
{
	((wxSizer*)_obj)->RecalcSizes();
}
	
EWXWEXPORT(void, wxSizer_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxSizer*)_obj)->CalcMin();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void, wxSizer_Layout)(void* _obj)
{
	((wxSizer*)_obj)->Layout();
}
	
EWXWEXPORT(void, wxSizer_Fit)(void* _obj, void* window )
{
	((wxSizer*)_obj)->Fit((wxWindow*) window);
}
	
EWXWEXPORT(void, wxSizer_SetSizeHints)(void* _obj, void* window )
{
	((wxSizer*)_obj)->SetSizeHints((wxWindow*) window);
}
	
EWXWEXPORT(int, wxSizer_GetChildren)(void* _obj, void* _res, int _cnt)
{
	if (_res && (unsigned int)_cnt == ((wxSizer*)_obj)->GetChildren().GetCount())
	{
		int i = 0;
		wxNode* node = NULL;
		wxList list = ((wxSizer*)_obj)->GetChildren();
	
		
		while ((node = list.Item(i++)))
			((void**)_res)[i] = (void*)node->GetData();

		return i;
	}
	else
		return ((wxSizer*)_obj)->GetChildren().GetCount();
}
	
EWXWEXPORT(void, wxSizer_SetDimension)(void* _obj, int x, int y, int width, int height)
{
	((wxSizer*)_obj)->SetDimension(x, y, width, height);
}
	
EWXWEXPORT(void*, wxGridSizer_Create)(int rows, int cols, int vgap, int hgap)
{
	return (void*) new wxGridSizer(rows, cols, vgap, hgap);
}
	
EWXWEXPORT(void, wxGridSizer_RecalcSizes)(void* _obj)
{
	((wxGridSizer*)_obj)->RecalcSizes();
}
	
EWXWEXPORT(void, wxGridSizer_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxGridSizer*)_obj)->CalcMin();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void, wxGridSizer_SetCols)(void* _obj, int cols )
{
	((wxGridSizer*)_obj)->SetCols(cols);
}
	
EWXWEXPORT(void, wxGridSizer_SetRows)(void* _obj, int rows)
{
	((wxGridSizer*)_obj)->SetRows(rows);
}
	
EWXWEXPORT(void, wxGridSizer_SetVGap)(void* _obj, int gap)
{
	((wxGridSizer*)_obj)->SetVGap(gap);
}
	
EWXWEXPORT(void, wxGridSizer_SetHGap)(void* _obj, int gap)
{
	((wxGridSizer*)_obj)->SetHGap(gap);
}
	
EWXWEXPORT(int, wxGridSizer_GetCols)(void* _obj)
{
	return ((wxGridSizer*)_obj)->GetCols();
}
	
EWXWEXPORT(int, wxGridSizer_GetRows)(void* _obj)
{
	return ((wxGridSizer*)_obj)->GetRows();
}
	
EWXWEXPORT(int, wxGridSizer_GetVGap)(void* _obj)
{
	return ((wxGridSizer*)_obj)->GetVGap();
}
	
EWXWEXPORT(int, wxGridSizer_GetHGap)(void* _obj)
{
	return ((wxGridSizer*)_obj)->GetHGap();
}
	
EWXWEXPORT(void*, wxFlexGridSizer_Create)(int rows, int cols, int vgap, int hgap)
{
	return new wxFlexGridSizer(rows, cols, vgap, hgap);
}
	
EWXWEXPORT(void, wxFlexGridSizer_RecalcSizes)(void* _obj)
{
	((wxFlexGridSizer*)_obj)->RecalcSizes();
}
	
EWXWEXPORT(void, wxFlexGridSizer_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxFlexGridSizer*)_obj)->CalcMin();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void, wxFlexGridSizer_AddGrowableRow)(void* _obj, size_t idx)
{
	((wxFlexGridSizer*)_obj)->AddGrowableRow(idx);
}
	
EWXWEXPORT(void, wxFlexGridSizer_RemoveGrowableRow)(void* _obj, size_t idx)
{
	((wxFlexGridSizer*)_obj)->RemoveGrowableRow(idx);
}
	
EWXWEXPORT(void, wxFlexGridSizer_AddGrowableCol)(void* _obj, size_t idx)
{
	((wxFlexGridSizer*)_obj)->AddGrowableCol(idx);
}
	
EWXWEXPORT(void, wxFlexGridSizer_RemoveGrowableCol)(void* _obj, size_t idx)
{
	((wxFlexGridSizer*)_obj)->RemoveGrowableCol(idx);
}
	
EWXWEXPORT(void*, wxBoxSizer_Create)(int orient )
{
	return (void*) new wxBoxSizer(orient);
}
	
EWXWEXPORT(void, wxBoxSizer_RecalcSizes)(void* _obj)
{
	((wxBoxSizer*)_obj)->RecalcSizes();
}
	
EWXWEXPORT(void, wxBoxSizer_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxBoxSizer*)_obj)->CalcMin();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(int, wxBoxSizer_GetOrientation)(void* _obj)
{
	return ((wxBoxSizer*)_obj)->GetOrientation();
}
	
EWXWEXPORT(void*, wxStaticBoxSizer_Create)(void* box, int orient)
{
	return (void*) new wxStaticBoxSizer((wxStaticBox*) box, orient );
}
	
EWXWEXPORT(void, wxStaticBoxSizer_RecalcSizes)(void* _obj)
{
	((wxStaticBoxSizer*)_obj)->RecalcSizes();
}
	
EWXWEXPORT(void, wxStaticBoxSizer_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxStaticBoxSizer*)_obj)->CalcMin();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void*, wxStaticBoxSizer_GetStaticBox)(void* _obj)
{
	return (void*)((wxStaticBoxSizer*)_obj)->GetStaticBox();
}
	
EWXWEXPORT(void*, wxNotebookSizer_Create)(void* nb)
{
	return (void*) new wxNotebookSizer((wxNotebook*)nb);
}
	
EWXWEXPORT(void, wxNotebookSizer_RecalcSizes)(void* _obj)
{
	((wxNotebookSizer*)_obj)->RecalcSizes();
}
	
EWXWEXPORT(void, wxNotebookSizer_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxNotebookSizer*)_obj)->CalcMin();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void*, wxNotebookSizer_GetNotebook)(void* _obj)
{
	return (void*)((wxNotebookSizer*)_obj)->GetNotebook();
}
	
}
