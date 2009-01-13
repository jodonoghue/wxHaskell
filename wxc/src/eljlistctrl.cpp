#include "wrapper.h"

extern "C"
{

typedef int _cdecl (*EiffelSortFunc) (void* obj, int data1, int data2);

typedef struct _EiffelSort
{
	void*          obj;
	EiffelSortFunc fnc;
}EiffelSort;

int wxCALLBACK ListCmp (long item1, long item2, long sortData)
{
	return ((EiffelSort*)sortData)->fnc (((EiffelSort*)sortData)->obj, (int)item1, (int)item2);
}

EWXWEXPORT(void*,wxListItem_Create)()
{
	return (void*) new wxListItem();
}

EWXWEXPORT(void,wxListItem_Delete)(void* _obj)
{
	delete (wxListItem*)_obj;
}
EWXWEXPORT(void,wxListItem_Clear)(void* _obj)
{
	((wxListItem*)_obj)->Clear();
}
	
EWXWEXPORT(void,wxListItem_ClearAttributes)(void* _obj)
{
	((wxListItem*)_obj)->ClearAttributes();
}
	
EWXWEXPORT(void,wxListItem_SetMask)(void* _obj,int mask)
{
	((wxListItem*)_obj)->SetMask((long)mask);
}
	
EWXWEXPORT(void,wxListItem_SetId)(void* _obj,int id)
{
	((wxListItem*)_obj)->SetId((long)id);
}
	
EWXWEXPORT(void,wxListItem_SetColumn)(void* _obj,int col)
{
	((wxListItem*)_obj)->SetColumn(col);
}
	
EWXWEXPORT(void,wxListItem_SetState)(void* _obj,int state)
{
	((wxListItem*)_obj)->SetState((long)state);
}
	
EWXWEXPORT(void,wxListItem_SetStateMask)(void* _obj,int stateMask)
{
	((wxListItem*)_obj)->SetStateMask((long)stateMask);
}
	
EWXWEXPORT(void,wxListItem_SetText)(void* _obj,wxString* text)
{
	((wxListItem*)_obj)->SetText(*text);
}
	
EWXWEXPORT(void,wxListItem_SetImage)(void* _obj,int image)
{
	((wxListItem*)_obj)->SetImage(image);
}
	
EWXWEXPORT(void,wxListItem_SetData)(void* _obj,int data)
{
	((wxListItem*)_obj)->SetData((long)data);
}
	
EWXWEXPORT(void,wxListItem_SetDataPointer)(void* _obj,void* data)
{
	((wxListItem*)_obj)->SetData(data);
}
	
EWXWEXPORT(void,wxListItem_SetWidth)(void* _obj,int width)
{
	((wxListItem*)_obj)->SetWidth(width);
}
	
EWXWEXPORT(void,wxListItem_SetAlign)(void* _obj,int align)
{
	((wxListItem*)_obj)->SetAlign((wxListColumnFormat)align);
}
	
EWXWEXPORT(void,wxListItem_SetTextColour)(void* _obj,void* colText)
{
	((wxListItem*)_obj)->SetTextColour(*((wxColour*)colText));
}
	
EWXWEXPORT(void,wxListItem_SetBackgroundColour)(void* _obj,void* colBack)
{
	((wxListItem*)_obj)->SetBackgroundColour(*((wxColour*)colBack));
}
	
EWXWEXPORT(void,wxListItem_SetFont)(void* _obj,void* font)
{
	((wxListItem*)_obj)->SetFont(*((wxFont*)font));
}
	
EWXWEXPORT(int,wxListItem_GetMask)(wxListItem* _obj)
{
	return (int)_obj->GetMask();
}
	
EWXWEXPORT(int,wxListItem_GetId)(wxListItem* _obj)
{
	return (int)_obj->GetId();
}
	
EWXWEXPORT(int,wxListItem_GetColumn)(void* _obj)
{
	return ((wxListItem*)_obj)->GetColumn();
}
	
EWXWEXPORT(int,wxListItem_GetState)(wxListItem* _obj)
{
	return (int)_obj->GetState();
}
	
EWXWEXPORT(wxString*,wxListItem_GetText)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxListItem*)_obj)->GetText();
	return result;
}
	
EWXWEXPORT(int,wxListItem_GetImage)(void* _obj)
{
	return ((wxListItem*)_obj)->GetImage();
}
	
EWXWEXPORT(int,wxListItem_GetData)(wxListItem* _obj)
{
	return (int)_obj->GetData();
}
	
EWXWEXPORT(int,wxListItem_GetWidth)(void* _obj)
{
	return ((wxListItem*)_obj)->GetWidth();
}
	
EWXWEXPORT(int,wxListItem_GetAlign)(wxListItem* _obj)
{
	return (int)_obj->GetAlign();
}
	
EWXWEXPORT(void*,wxListItem_GetAttributes)(void* _obj)
{
	return (void*)((wxListItem*)_obj)->GetAttributes();
}
	
EWXWEXPORT(int,wxListItem_HasAttributes)(wxListItem* _obj)
{
	return (int)_obj->HasAttributes();
}
	
EWXWEXPORT(void,wxListItem_GetTextColour)(void* _obj,void* _ref)
{
	*((wxColour*)_ref) = ((wxListItem*)_obj)->GetTextColour();
}
	
EWXWEXPORT(void,wxListItem_GetBackgroundColour)(void* _obj,void* _ref)
{
	*((wxColour*)_ref) = ((wxListItem*)_obj)->GetBackgroundColour();
}
	
EWXWEXPORT(void,wxListItem_GetFont)(void* _obj,void* _ref)
{
	*((wxFont*)_ref) = ((wxListItem*)_obj)->GetFont();
}
	
EWXWEXPORT(void*,wxListCtrl_Create)(wxWindow* _prt,int _id, int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxListCtrl (_prt, _id,wxPoint(_lft, _top),wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int,wxListCtrl_SetForegroundColour)(wxListCtrl* _obj,wxColour* col)
{
	return (int)_obj->SetForegroundColour(*col);
}
	
EWXWEXPORT(int,wxListCtrl_SetBackgroundColour)(wxListCtrl* _obj,void* col)
{
	return (int)_obj->SetBackgroundColour(*((wxColour*)col));
}
	
EWXWEXPORT(int,wxListCtrl_GetColumn)(wxListCtrl* _obj,int col,void* item)
{
	return (int)_obj->GetColumn(col, *((wxListItem*)item));
}
	
EWXWEXPORT(int,wxListCtrl_SetColumn)(wxListCtrl* _obj,int col,void* item)
{
	return (int)_obj->SetColumn(col, *((wxListItem*)item));
}
	
EWXWEXPORT(int,wxListCtrl_GetColumnWidth)(void* _obj,int col)
{
	return ((wxListCtrl*)_obj)->GetColumnWidth(col);
}
	
EWXWEXPORT(int,wxListCtrl_SetColumnWidth)(wxListCtrl* _obj,int col,int width)
{
	return (int)_obj->SetColumnWidth(col, width);
}
	
EWXWEXPORT(int,wxListCtrl_GetCountPerPage)(void* _obj)
{
	return ((wxListCtrl*)_obj)->GetCountPerPage();
}
	
EWXWEXPORT(void*,wxListCtrl_GetEditControl)(void* _obj)
{
#ifdef __WIN32__
	return (void*)((wxListCtrl*)_obj)->GetEditControl();
#else
	return NULL;
#endif
}
	
EWXWEXPORT(int,wxListCtrl_GetItem)(wxListCtrl* _obj,void* info)
{
	return (int)_obj->GetItem(*((wxListItem*)info));
}
	
EWXWEXPORT(int,wxListCtrl_SetItemFromInfo)(wxListCtrl* _obj,void* info)
{
	return (int)_obj->SetItem(*((wxListItem*)info));
}
	
EWXWEXPORT(int,wxListCtrl_SetItem)(wxListCtrl* _obj,int index,int col,wxString* label,int imageId)
{
	return (int) _obj->SetItem((long)index, col, *label, imageId);
}
	
EWXWEXPORT(int,wxListCtrl_GetItemState)(void* _obj,int item,int stateMask)
{
	return ((wxListCtrl*)_obj)->GetItemState((long)item, (long)stateMask);
}
	
EWXWEXPORT(int,wxListCtrl_SetItemState)(wxListCtrl* _obj,int item,int state,int stateMask)
{
	return (int)_obj->SetItemState((long)item, (long)state, (long)stateMask);
}
	
EWXWEXPORT(int,wxListCtrl_SetItemImage)(wxListCtrl* _obj,int item,int image,int selImage)
{
	return (int)_obj->SetItemImage((long)item, image, selImage);
}
	
EWXWEXPORT(wxString*,wxListCtrl_GetItemText)(void* _obj,int item)
{
	wxString *result = new wxString();
	*result = ((wxListCtrl*)_obj)->GetItemText((long)item);
	return result;
}
	
EWXWEXPORT(void,wxListCtrl_SetItemText)(void* _obj,int item,wxString* str)
{
	((wxListCtrl*)_obj)->SetItemText((long)item, *str);
}
	
EWXWEXPORT(int,wxListCtrl_GetItemData)(wxListCtrl* _obj,int item)
{
	return (int)_obj->GetItemData((long)item);
}
	
EWXWEXPORT(int,wxListCtrl_SetItemData)(wxListCtrl* _obj,int item,int data)
{
	return (int)_obj->SetItemData((long)item, (long)data);
}
	
EWXWEXPORT(int, wxListCtrl_GetItemRect)(void* _obj, int item, int code, void* x, void* y, void* w, void* h)
{
	wxRect rct;
	int result = (int)((wxListCtrl*)_obj)->GetItemRect((long)item, rct, code);
	if (result)
	{
		*((int*)x) = rct.x;
		*((int*)y) = rct.y;
		*((int*)w) = rct.width;
		*((int*)h) = rct.height;
	} 
	return result;
}
	
EWXWEXPORT(int, wxListCtrl_GetItemPosition)(void* _obj, int item, void* x, void* y)
{
	wxPoint pos;
	int result = (int)((wxListCtrl*)_obj)->GetItemPosition((long)item, pos);
	*((int*)x) = pos.x;
	*((int*)y) = pos.y;

	return result;
}
	
EWXWEXPORT(int,wxListCtrl_SetItemPosition)(void* _obj,int item,int x,int y)
{
  return ((wxListCtrl*)_obj)->SetItemPosition((long)item, wxPoint(x,y));
}
	
EWXWEXPORT(int,wxListCtrl_GetItemCount)(void* _obj)
{
	return ((wxListCtrl*)_obj)->GetItemCount();
}
	
EWXWEXPORT(int,wxListCtrl_GetColumnCount)(void* _obj)
{
	return ((wxListCtrl*)_obj)->GetColumnCount();
}
	
EWXWEXPORT(void,wxListCtrl_GetItemSpacing)(void* _obj,int isSmall,int* h,int* w)
{
#if (wxVERSION_NUMBER <= 2600)
	int x = ((wxListCtrl*)_obj)->GetItemSpacing(isSmall != 0);
        *h = x;
        *w = x;
#else
	wxSize res = ((wxListCtrl*)_obj)->GetItemSpacing();
	*h = res.GetHeight();
	*w = res.GetWidth();
#endif
}
	
EWXWEXPORT(int,wxListCtrl_GetSelectedItemCount)(void* _obj)
{
	return ((wxListCtrl*)_obj)->GetSelectedItemCount();
}
	
EWXWEXPORT(void,wxListCtrl_GetTextColour)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxListCtrl*)_obj)->GetTextColour();
}
	
EWXWEXPORT(void,wxListCtrl_SetTextColour)(void* _obj,void* col)
{
	((wxListCtrl*)_obj)->SetTextColour(*((wxColour*)col));
}
	
EWXWEXPORT(int,wxListCtrl_GetTopItem)(wxListCtrl* _obj)
{
	return (int)_obj->GetTopItem();
}
	
EWXWEXPORT(void,wxListCtrl_SetSingleStyle)(void* _obj,int style,int add)
{
	((wxListCtrl*)_obj)->SetSingleStyle((long)style, add != 0);
}
	
EWXWEXPORT(void,wxListCtrl_SetWindowStyleFlag)(void* _obj,int style)
{
	((wxListCtrl*)_obj)->SetWindowStyleFlag((long)style);
}
	
EWXWEXPORT(int,wxListCtrl_GetNextItem)(void* _obj,int item,int geometry,int state)
{
	return ((wxListCtrl*)_obj)->GetNextItem((long)item, geometry, state);
}
	
EWXWEXPORT(void*,wxListCtrl_GetImageList)(void* _obj,int which)
{
	return (void*)((wxListCtrl*)_obj)->GetImageList(which);
}
	
EWXWEXPORT(void,wxListCtrl_SetImageList)(void* _obj,void* imageList,int which)
{
	((wxListCtrl*)_obj)->SetImageList((wxImageList*)imageList, which);
}
	
EWXWEXPORT(int,wxListCtrl_Arrange)(wxListCtrl* _obj,int flag)
{
	return (int)_obj->Arrange(flag);
}
	
EWXWEXPORT(int,wxListCtrl_DeleteItem)(wxListCtrl* _obj,int item)
{
	return (int)_obj->DeleteItem((long)item);
}
	
EWXWEXPORT(int,wxListCtrl_DeleteAllItems)(wxListCtrl* _obj)
{
	return (int)_obj->DeleteAllItems();
}
	
EWXWEXPORT(int,wxListCtrl_DeleteColumn)(wxListCtrl* _obj,int col)
{
	return (int)_obj->DeleteColumn(col);
}
	
EWXWEXPORT(int,wxListCtrl_DeleteAllColumns)(wxListCtrl* _obj)
{
	return (int)_obj->DeleteAllColumns();
}
	
EWXWEXPORT(void,wxListCtrl_ClearAll)(void* _obj)
{
	((wxListCtrl*)_obj)->ClearAll();
}
	
EWXWEXPORT(void,wxListCtrl_EditLabel)(void* _obj,int item)
{
	((wxListCtrl*)_obj)->EditLabel((long)item);
}
	
EWXWEXPORT(int,wxListCtrl_EndEditLabel)(wxListCtrl* _obj,int cancel)
{
#ifdef __WIN32__
	return (int)_obj->EndEditLabel(cancel != 0);
#else
	return 0;
#endif
}
	
EWXWEXPORT(int,wxListCtrl_EnsureVisible)(wxListCtrl* _obj,int item)
{
	return (int)_obj->EnsureVisible((long)item);
}
	
EWXWEXPORT(int,wxListCtrl_FindItem)(void* _obj,int start,wxString* str,int partial)
{
	return (long)((wxListCtrl*)_obj)->FindItem((long)start, * str, partial != 0);
}
	
EWXWEXPORT(int,wxListCtrl_FindItemByData)(wxListCtrl* _obj,int start,int data)
{
	return (int)_obj->FindItem((long)start, (long)data);
}
	
EWXWEXPORT(int,wxListCtrl_FindItemByPosition)(wxListCtrl* _obj,int start,int x,int y,int direction)
{
	return (int)_obj->FindItem((long)start, wxPoint(x, y), direction);
}
	
EWXWEXPORT(int,wxListCtrl_HitTest)(void* _obj,int x,int y,void* flags)
{
	return ((wxListCtrl*)_obj)->HitTest(wxPoint(x, y), *((int*)flags));
}
	
EWXWEXPORT(int,wxListCtrl_InsertItem)(wxListCtrl* _obj,void* info)
{
	return (int)_obj->InsertItem(*((wxListItem*)info));
}
	
EWXWEXPORT(int,wxListCtrl_InsertItemWithData)(wxListCtrl* _obj,int index,wxString* label)
{
	return (int)_obj->InsertItem((long)index, *label);
}
	
EWXWEXPORT(int,wxListCtrl_InsertItemWithImage)(wxListCtrl* _obj,int index,int imageIndex)
{
	return (int)_obj->InsertItem((long)index, imageIndex);
}
	
EWXWEXPORT(int,wxListCtrl_InsertItemWithLabel)(wxListCtrl* _obj,int index,wxString* label,int imageIndex)
{
	return (int)_obj->InsertItem((long)index, *label, imageIndex);
}
	
EWXWEXPORT(int,wxListCtrl_InsertColumnFromInfo)(wxListCtrl* _obj,int col,void* info)
{
	return (int)_obj->InsertColumn((long)col, *((wxListItem*)info));
}
	
EWXWEXPORT(int,wxListCtrl_InsertColumn)(wxListCtrl* _obj,int col,wxString* heading,int format,int width)
{
	return (int)_obj->InsertColumn((long)col, * heading, format, width);
}
	
EWXWEXPORT(int,wxListCtrl_ScrollList)(wxListCtrl* _obj,int dx,int dy)
{
	return (int)_obj->ScrollList(dx, dy);
}
	
EWXWEXPORT(int,wxListCtrl_SortItems)(wxListCtrl* _obj,void* fnc,void* obj)
{
	EiffelSort srt = {obj, (EiffelSortFunc)fnc};
	return (int)_obj->SortItems(ListCmp, (long)&srt);
}
	
EWXWEXPORT(void,wxListCtrl_UpdateStyle)(void* _obj)
{
#ifdef __WIN32__
	((wxListCtrl*)_obj)->UpdateStyle();
#endif
}
	
}
