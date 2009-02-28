#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxSizerItem_Create)(int width,int height,int option,int flag,int border,void* userData)
{
	return (void*)new wxSizerItem(width, height, option, flag, border, new ELJDataObject(userData));
}
	
EWXWEXPORT(void*,wxSizerItem_CreateInWindow)(wxWindow* window,int option,int flag,int border,void* userData)
{
	return (void*)new wxSizerItem(window, option, flag, border, new ELJDataObject(userData));
}
	
EWXWEXPORT(void*,wxSizerItem_CreateInSizer)(wxSizer* sizer,int option,int flag,int border,void* userData)
{
	return (void*)new wxSizerItem(sizer, option, flag, border, new ELJDataObject(userData));
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
	
EWXWEXPORT(void,wxSizerItem_SetRatio)(wxSizerItem* self,int width,int height)
{
	self->SetRatio(width, height);
}
	
EWXWEXPORT(void,wxSizerItem_SetFloatRatio)(wxSizerItem* self,float ratio)
{
	self->SetRatio(ratio);
}
	
EWXWEXPORT(float,wxSizerItem_GetRatio)(wxSizerItem* self)
{
	return self->GetRatio();
}
	
EWXWEXPORT(bool,wxSizerItem_IsWindow)(wxSizerItem* self)
{
	return self->IsWindow();
}
	
EWXWEXPORT(bool,wxSizerItem_IsSizer)(wxSizerItem* self)
{
	return self->IsSizer();
}
	
EWXWEXPORT(bool,wxSizerItem_IsSpacer)(wxSizerItem* self)
{
	return self->IsSpacer();
}
	
EWXWEXPORT(void,wxSizerItem_SetInitSize)(wxSizerItem* self,int x,int y)
{
	self->SetInitSize(x, y);
}
	
#if (wxVERSION_NUMBER < 2800)	
EWXWEXPORT(void,wxSizerItem_SetOption)(wxSizerItem* self,int option)
{
	self->SetOption(option);
}
#endif

EWXWEXPORT(void,wxSizerItem_SetFlag)(wxSizerItem* self,int flag)
{
	self->SetFlag(flag);
}
	
EWXWEXPORT(void,wxSizerItem_SetBorder)(wxSizerItem* self,int border)
{
	self->SetBorder(border);
}
	
EWXWEXPORT(wxWindow*,wxSizerItem_GetWindow)(wxSizerItem* self)
{
	return self->GetWindow();
}
	
EWXWEXPORT(void,wxSizerItem_SetWindow)(wxSizerItem* self,wxWindow* window)
{
	self->SetWindow(window);
}
	
EWXWEXPORT(void*,wxSizerItem_GetSizer)(wxSizerItem* self)
{
	return (void*)self->GetSizer();
}
	
EWXWEXPORT(void,wxSizerItem_SetSizer)(wxSizerItem* self,wxSizer* sizer)
{
	self->SetSizer(sizer);
}
	
#if (wxVERSION_NUMBER < 2800)
EWXWEXPORT(int,wxSizerItem_GetOption)(wxSizerItem* self)
{
	return self->GetOption();
}
#endif

EWXWEXPORT(int,wxSizerItem_GetFlag)(wxSizerItem* self)
{
	return self->GetFlag();
}
	
EWXWEXPORT(int,wxSizerItem_GetBorder)(wxSizerItem* self)
{
	return self->GetBorder();
}
	
EWXWEXPORT(void*,wxSizerItem_GetUserData)(wxSizerItem* self)
{
	return ((ELJDataObject*)self->GetUserData())->data;
}
	
EWXWEXPORT(void, wxSizerItem_GetPosition)(void* _obj, void* _x, void* _y)
{
	wxPoint res = ((wxSizerItem*)_obj)->GetPosition();
	(*(int*)_x) = res.x;
	(*(int*)_y) = res.y;
}
	
#if (wxVERSION_NUMBER >= 2800)
EWXWEXPORT(void,wxSizerItem_Delete)(wxSizerItem* self)
{
  delete self;
}

EWXWEXPORT(void,wxSizerItem_DeleteWindows)(wxSizerItem* self)
{
  self->DeleteWindows();
}

EWXWEXPORT(void,wxSizerItem_DetachSizer)(wxSizerItem* self)
{
  self->DetachSizer();
}

EWXWEXPORT(int,wxSizerItem_GetProportion)(wxSizerItem* self)
{
  return self->GetProportion();
}

EWXWEXPORT(void, wxSizerItem_GetRect)(void *_obj, void *_x, void *_y, void *_w, void *_h)
{
  wxRect r = ((wxSizerItem*)_obj)->GetRect();

  (*(int *)_x) = r.GetX();
  (*(int *)_y) = r.GetY();
  (*(int *)_w) = r.GetWidth();
  (*(int *)_h) = r.GetHeight();
}

EWXWEXPORT(void, wxSizerItem_GetSpacer)(void *_obj, void *_w, void *_h)
{
  wxSize sz(0,0);

  if (((wxSizerItem*)_obj)->IsSpacer())
  {
    sz = ((wxSizerItem*)_obj)->GetSpacer();
  }
  (*(int *)_w) = sz.GetWidth();
  (*(int *)_h) = sz.GetHeight();
}

EWXWEXPORT(bool,wxSizerItem_IsShown)(wxSizerItem* self)
{
  return  self->IsShown();
}

EWXWEXPORT(void,wxSizerItem_SetProportion)(wxSizerItem* self,int proportion)
{
  self->SetProportion(proportion);
}

EWXWEXPORT(void,wxSizerItem_SetSpacer)(wxSizerItem* self,int width,int height)
{
  self->SetSpacer(wxSize(width, height));
}

EWXWEXPORT(void,wxSizerItem_Show)(wxSizerItem* self,int show)
{
  self->Show(show);
}
#endif

EWXWEXPORT(void,wxSizer_AddWindow)(wxSizer* self,wxWindow* window,int option,int flag,int border,void* userData)
{
	self->Add(window, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_AddSizer)(wxSizer* self,wxSizer* sizer,int option,int flag,int border,void* userData)
{
	self->Add(sizer, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_Add)(wxSizer* self,int width,int height,int option,int flag,int border,void* userData)
{
	self->Add(width, height, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_InsertWindow)(wxSizer* self,int before,wxWindow* window,int option,int flag,int border,void* userData)
{
	self->Insert(before, window, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_InsertSizer)(wxSizer* self,int before,wxSizer* sizer,int option,int flag,int border,void* userData)
{
	self->Insert(before, sizer, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_Insert)(wxSizer* self,int before,int width,int height,int option,int flag,int border,void* userData)
{
	self->Insert(before, width, height, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_PrependWindow)(wxSizer* self,wxWindow* window,int option,int flag,int border,void* userData)
{
	self->Prepend(window, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_PrependSizer)(wxSizer* self,wxSizer* sizer,int option,int flag,int border,void* userData)
{
	self->Prepend(sizer, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_Prepend)(wxSizer* self,int width,int height,int option,int flag,int border,void* userData)
{
	self->Prepend(width, height, option, flag, border, new ELJDataObject (userData));
}
	
#if (wxVERSION_NUMBER < 2800)	
EWXWEXPORT(bool,wxSizer_RemoveWindow)(wxSizer* self,wxWindow* window)
{
	return self->Remove(window);
}
	
EWXWEXPORT(bool,wxSizer_RemoveSizer)(wxSizer* self,wxSizer* sizer)
{
	return self->Remove(sizer);
}
	
EWXWEXPORT(bool,wxSizer_Remove)(wxSizer* self,int pos)
{
	return self->Remove(pos);
}
#endif
	
EWXWEXPORT(void,wxSizer_SetMinSize)(wxSizer* self,int width,int height)
{
	self->SetMinSize(width, height);
}
	
EWXWEXPORT(void,wxSizer_SetItemMinSizeWindow)(wxSizer* self,wxWindow* window,int width,int height)
{
	self->SetItemMinSize( window, width, height);
}
	
EWXWEXPORT(void,wxSizer_SetItemMinSizeSizer)(wxSizer* self,wxSizer* sizer,int width,int height)
{
	self->SetItemMinSize(sizer, width, height);
}
	
EWXWEXPORT(void,wxSizer_SetItemMinSize)(wxSizer* self,int pos,int width,int height)
{
	self->SetItemMinSize(pos, width, height);
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
	
EWXWEXPORT(void,wxSizer_RecalcSizes)(wxSizer* self)
{
	self->RecalcSizes();
}
	
EWXWEXPORT(void, wxSizer_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxSizer*)_obj)->CalcMin();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void,wxSizer_Layout)(wxSizer* self)
{
	self->Layout();
}
	
EWXWEXPORT(void,wxSizer_Fit)(wxSizer* self,wxWindow* window)
{
	self->Fit(window);
}
	
EWXWEXPORT(void,wxSizer_SetSizeHints)(wxSizer* self,wxWindow* window)
{
	self->SetSizeHints(window);
}
	
EWXWEXPORT(int, wxSizer_GetChildren)(wxSizer* self, void* _res, int _cnt)
{
	if (_res && (unsigned int)_cnt == self->GetChildren().GetCount())
	{
		int i = 0;
		wxSizerItemList::compatibility_iterator node = self->GetChildren().GetFirst();
		while (node)
		{
			((void**)_res)[i] = node->GetData();
			node = node->GetNext();
			++i;
		}
		return i;
	}
	else
		return self->GetChildren().GetCount();
}

#if (wxVERSION_NUMBER >= 2800)
EWXWEXPORT(void,wxSizer_AddSpacer)(wxSizer* self,int size)
{
  self->AddSpacer(size);
}

EWXWEXPORT(void,wxSizer_AddStretchSpacer)(wxSizer* self,int size)
{
  self->AddStretchSpacer(size);
}

EWXWEXPORT(void,wxSizer_Clear)(wxSizer* self,bool delete_windows)
{
  self->Clear(delete_windows);
}

EWXWEXPORT(bool,wxSizer_DetachWindow)(wxSizer* self,wxWindow* window)
{
  return self->Detach(window);
}

EWXWEXPORT(bool,wxSizer_DetachSizer)(wxSizer* self,wxSizer* sizer)
{
  return self->Detach(sizer);
}

EWXWEXPORT(bool,wxSizer_Detach)(wxSizer* self,int index)
{
  return self->Detach((size_t) index);
}

EWXWEXPORT(void,wxSizer_FitInside)(wxSizer* self,wxWindow* window)
{
  self->FitInside( window);
}

EWXWEXPORT(void*,wxSizer_GetContainingWindow)(wxSizer* self)
{
  return (void*)self->GetContainingWindow();
}

EWXWEXPORT(void*,wxSizer_GetItemWindow)(wxSizer* self,wxWindow* window,bool recursive)
{
  return (void*)self->GetItem( window, recursive);
}

EWXWEXPORT(void*,wxSizer_GetItemSizer)(wxSizer* self,wxSizer* sizer,bool recursive)
{
  return (void*)self->GetItem(sizer, recursive);
}

EWXWEXPORT(void*,wxSizer_GetItem)(wxSizer* self,int index)
{
  return (void*)self->GetItem((size_t) index);
}

EWXWEXPORT(bool,wxSizer_HideWindow)(wxSizer* self,wxWindow* window)
{
  return self->Hide(window);
}

EWXWEXPORT(bool,wxSizer_HideSizer)(wxSizer* self,wxSizer* sizer)
{
  return self->Hide(sizer);
}

EWXWEXPORT(bool,wxSizer_Hide)(wxSizer* self,int index)
{
  return self->Hide((size_t) index);
}

EWXWEXPORT(void*,wxSizer_InsertSpacer)(wxSizer* self,int index,int size)
{
  return (void*)self->InsertSpacer((size_t) index, size);
}

EWXWEXPORT(void*,wxSizer_InsertStretchSpacer)(wxSizer* self,int index,int prop)
{
  return (void*)self->InsertStretchSpacer((size_t) index, prop);
}

EWXWEXPORT(bool,wxSizer_IsShownWindow)(wxSizer* self,wxWindow* window)
{
  return self->IsShown( window);
}

EWXWEXPORT(bool,wxSizer_IsShownSizer)(wxSizer* self,wxSizer* sizer)
{
  return self->IsShown(sizer);
}

EWXWEXPORT(bool,wxSizer_IsShown)(wxSizer* self,size_t index)
{
  return self->IsShown( index);
}

EWXWEXPORT(void*,wxSizer_PrependSpacer)(wxSizer* self,int size)
{
  return (void*)self->PrependSpacer(size);
}

EWXWEXPORT(void*,wxSizer_PrependStretchSpacer)(wxSizer* self,int prop)
{
  return (void*)self->PrependStretchSpacer(prop);
}

EWXWEXPORT(bool,wxSizer_ReplaceWindow)(wxSizer* self,wxWindow* oldwin,wxWindow* newwin,bool recursive)
{
  return self->Replace(oldwin, newwin, recursive);
}

EWXWEXPORT(bool,wxSizer_ReplaceSizer)(wxSizer* self,wxSizer* oldsz,wxSizer* newsz,bool recursive)
{
  return self->Replace( oldsz, newsz,recursive);
}

EWXWEXPORT(bool,wxSizer_Replace)(wxSizer* self,int oldindex,wxSizerItem* newsz)
{
  return self->Replace((size_t) oldindex,newsz);
}

EWXWEXPORT(void,wxSizer_SetVirtualSizeHints)(wxSizer* self,wxWindow* window)
{
  self->SetVirtualSizeHints(window);
}

EWXWEXPORT(bool,wxSizer_ShowWindow)(wxSizer* self,wxWindow* window,bool show,bool recursive)
{
  return self->Show(window, show, recursive);
}

EWXWEXPORT(bool,wxSizer_ShowSizer)(wxSizer* self,wxSizer* sizer,bool show,bool recursive)
{
  return self->Show(sizer, show, recursive);
}

EWXWEXPORT(bool,wxSizer_Show)(wxSizer* self,int index,bool show)
{
  return self->Show((size_t) index, show);
}
#endif
	
EWXWEXPORT(void,wxSizer_SetDimension)(wxSizer* self,int x,int y,int width,int height)
{
	self->SetDimension(x, y, width, height);
}
	
EWXWEXPORT(void*,wxGridSizer_Create)(int rows,int cols,int vgap,int hgap)
{
	return (void*)new wxGridSizer(rows, cols, vgap, hgap);
}
	
EWXWEXPORT(void,wxGridSizer_RecalcSizes)(void* self)
{
	((wxGridSizer*)self)->RecalcSizes();
}
	
EWXWEXPORT(void, wxGridSizer_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxGridSizer*)_obj)->CalcMin();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void,wxGridSizer_SetCols)(void* self,int cols)
{
	((wxGridSizer*)self)->SetCols(cols);
}
	
EWXWEXPORT(void,wxGridSizer_SetRows)(void* self,int rows)
{
	((wxGridSizer*)self)->SetRows(rows);
}
	
EWXWEXPORT(void,wxGridSizer_SetVGap)(void* self,int gap)
{
	((wxGridSizer*)self)->SetVGap(gap);
}
	
EWXWEXPORT(void,wxGridSizer_SetHGap)(void* self,int gap)
{
	((wxGridSizer*)self)->SetHGap(gap);
}
	
EWXWEXPORT(int,wxGridSizer_GetCols)(void* self)
{
	return ((wxGridSizer*)self)->GetCols();
}
	
EWXWEXPORT(int,wxGridSizer_GetRows)(void* self)
{
	return ((wxGridSizer*)self)->GetRows();
}
	
EWXWEXPORT(int,wxGridSizer_GetVGap)(void* self)
{
	return ((wxGridSizer*)self)->GetVGap();
}
	
EWXWEXPORT(int,wxGridSizer_GetHGap)(void* self)
{
	return ((wxGridSizer*)self)->GetHGap();
}
	
EWXWEXPORT(void*,wxFlexGridSizer_Create)(int rows,int cols,int vgap,int hgap)
{
	return new wxFlexGridSizer(rows, cols, vgap, hgap);
}
	
EWXWEXPORT(void,wxFlexGridSizer_RecalcSizes)(void* self)
{
	((wxFlexGridSizer*)self)->RecalcSizes();
}
	
EWXWEXPORT(void, wxFlexGridSizer_CalcMin)(void* _obj, void* _w, void* _h)
{
	wxSize res = ((wxFlexGridSizer*)_obj)->CalcMin();
	(*(int*)_w) = res.GetWidth();
	(*(int*)_h) = res.GetHeight();
}
	
EWXWEXPORT(void,wxFlexGridSizer_AddGrowableRow)(void* self,size_t idx)
{
	((wxFlexGridSizer*)self)->AddGrowableRow(idx);
}
	
EWXWEXPORT(void,wxFlexGridSizer_RemoveGrowableRow)(void* self,size_t idx)
{
	((wxFlexGridSizer*)self)->RemoveGrowableRow(idx);
}
	
EWXWEXPORT(void,wxFlexGridSizer_AddGrowableCol)(void* self,size_t idx)
{
	((wxFlexGridSizer*)self)->AddGrowableCol(idx);
}
	
EWXWEXPORT(void,wxFlexGridSizer_RemoveGrowableCol)(void* self,size_t idx)
{
	((wxFlexGridSizer*)self)->RemoveGrowableCol(idx);
}
	
EWXWEXPORT(void*,wxBoxSizer_Create)(int orient)
{
	return (void*)new wxBoxSizer(orient);
}
	
EWXWEXPORT(void,wxBoxSizer_RecalcSizes)(void* self)
{
	((wxBoxSizer*)self)->RecalcSizes();
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
	
#if (wxVERSION_NUMBER < 2800)
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
#endif

}
