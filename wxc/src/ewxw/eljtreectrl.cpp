#include "wrapper.h"

class ELJTreeItemData : public wxTreeItemData
{
	public:
		void* data;
		ELJTreeItemData(void* _data) {data = _data;};
};

extern "C"
{

EWXWEXPORT(void*, wxTreeItemId_Create) ()
{
	return new wxTreeItemId();
}

EWXWEXPORT(void, wxTreeItemId_Delete) (void* _obj)
{
	delete (wxTreeItemId*)_obj;
}

EWXWEXPORT(int, wxTreeItemId_IsOk) (void* _obj)
{
	return (int)((wxTreeItemId*)_obj)->IsOk();
}

IMPLEMENT_DYNAMIC_CLASS(ELJTreeControl,wxTreeCtrl)


EWXWEXPORT(void*, wxTreeCtrl_Create) (void* _obj, void* _cmp, void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new ELJTreeControl (_obj, _cmp, (wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(int, wxTreeCtrl_GetCount)(void* _obj)
{
	int result =(int)((ELJTreeControl*)_obj)->GetCount();
#ifdef __WXGTK__
	wxTreeItemId t = ((ELJTreeControl*)_obj)->GetRootItem();
	if (t.IsOk()) result++;
#endif
	return result;
}
	
EWXWEXPORT(int, wxTreeCtrl_GetIndent)(void* _obj)
{
	return ((ELJTreeControl*)_obj)->GetIndent();
}
	
EWXWEXPORT(void, wxTreeCtrl_SetIndent)(void* _obj, int indent)
{
	((ELJTreeControl*)_obj)->SetIndent(indent);
}
	
EWXWEXPORT(int, wxTreeCtrl_GetSpacing)(void* _obj)
{
	return (int)((ELJTreeControl*)_obj)->GetSpacing();
}
	
EWXWEXPORT(void, wxTreeCtrl_SetSpacing)(void* _obj, int spacing)
{
	((ELJTreeControl*)_obj)->SetSpacing(spacing);
}
	
EWXWEXPORT(void*, wxTreeCtrl_GetImageList)(void* _obj)
{
	return (void*)((ELJTreeControl*)_obj)->GetImageList();
}
	
EWXWEXPORT(void*, wxTreeCtrl_GetStateImageList)(void* _obj)
{
	return (void*)((ELJTreeControl*)_obj)->GetStateImageList();
}
	
EWXWEXPORT(void, wxTreeCtrl_SetImageList)(void* _obj, void* imageList)
{
	((ELJTreeControl*)_obj)->SetImageList((wxImageList*) imageList);
}
	
EWXWEXPORT(void, wxTreeCtrl_SetStateImageList)(void* _obj, void* imageList)
{
	((ELJTreeControl*)_obj)->SetStateImageList((wxImageList*) imageList);
}
	
EWXWEXPORT(int, wxTreeCtrl_GetItemText)(void* _obj, void* item, void* _buf)
{
	wxString result = ((ELJTreeControl*)_obj)->GetItemText(*(wxTreeItemId*)item);
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(int, wxTreeCtrl_GetItemImage)(void* _obj, void* item, int which)
{
	return ((ELJTreeControl*)_obj)->GetItemImage(*((wxTreeItemId*) item), (wxTreeItemIcon) which);
}
	
EWXWEXPORT(void*, wxTreeCtrl_GetItemData)(void* _obj, void* item)
{
	return ((ELJTreeItemData*)((ELJTreeControl*)_obj)->GetItemData(*((wxTreeItemId*) item)))->data;
}
	
EWXWEXPORT(void, wxTreeCtrl_SetItemText)(void* _obj, void* item, void* text)
{
	((ELJTreeControl*)_obj)->SetItemText(*((wxTreeItemId*) item), (char*)text);
}
	
EWXWEXPORT(void, wxTreeCtrl_SetItemImage)(void* _obj, void* item, int image, int which)
{
	((ELJTreeControl*)_obj)->SetItemImage(*((wxTreeItemId*)item), image, (wxTreeItemIcon)which);
}
	
EWXWEXPORT(void, wxTreeCtrl_SetItemData)(void* _obj, void* item, void* data)
{
	((ELJTreeControl*)_obj)->SetItemData(*((wxTreeItemId*) item), new ELJTreeItemData (data));
}
	
EWXWEXPORT(void, wxTreeCtrl_SetItemHasChildren)(void* _obj, void* item, int has)
{
	((ELJTreeControl*)_obj)->SetItemHasChildren(*((wxTreeItemId*)item), has != 0);
}
	
EWXWEXPORT(void, wxTreeCtrl_SetItemBold)(void* _obj, void* item, int bold)
{
	((ELJTreeControl*)_obj)->SetItemBold(*((wxTreeItemId*) item), bold != 0);
}
	
EWXWEXPORT(void, wxTreeCtrl_SetItemDropHighlight)(void* _obj, void* item, int highlight)
{
#ifdef __WIN32__
	((ELJTreeControl*)_obj)->SetItemDropHighlight(*((wxTreeItemId*) item), highlight != 0);
#endif
}
	
EWXWEXPORT(void, wxTreeCtrl_SetItemTextColour)(void* _obj, void* item, void* col)
{
	((ELJTreeControl*)_obj)->SetItemTextColour(*((wxTreeItemId*)item), *((wxColour*) col));
}
	
EWXWEXPORT(void, wxTreeCtrl_SetItemBackgroundColour)(void* _obj, void* item, void* col)
{
	((ELJTreeControl*)_obj)->SetItemBackgroundColour(*((wxTreeItemId*) item), *((wxColour*) col));
}
	
EWXWEXPORT(void, wxTreeCtrl_SetItemFont)(void* _obj, void* item, void* font)
{
	((ELJTreeControl*)_obj)->SetItemFont(*((wxTreeItemId*) item), *((wxFont*) font));
}
	
EWXWEXPORT(int, wxTreeCtrl_IsVisible)(void* _obj, void* item)
{
	return (int)((ELJTreeControl*)_obj)->IsVisible(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(int, wxTreeCtrl_ItemHasChildren)(void* _obj, void* item)
{
	return (int)((ELJTreeControl*)_obj)->ItemHasChildren(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(int, wxTreeCtrl_IsExpanded)(void* _obj, void* item)
{
	return (int)((ELJTreeControl*)_obj)->IsExpanded(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(int, wxTreeCtrl_IsSelected)(void* _obj, void* item)
{
	return (int)((ELJTreeControl*)_obj)->IsSelected(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(int, wxTreeCtrl_IsBold)(void* _obj, void* item)
{
	return (int)((ELJTreeControl*)_obj)->IsBold(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(int, wxTreeCtrl_GetChildrenCount)(void* _obj, void* item, int recursively)
{
	return ((ELJTreeControl*)_obj)->GetChildrenCount(*((wxTreeItemId*) item), recursively);
}
	
EWXWEXPORT(void, wxTreeCtrl_GetRootItem)(void* _obj, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetRootItem();
}
	
EWXWEXPORT(void, wxTreeCtrl_GetSelection)(void* _obj, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetSelection();
}
	
EWXWEXPORT(int, wxTreeCtrl_GetSelections)(void* _obj, void* selections)
{
	int result = 0;
	wxArrayTreeItemIds sel;
	result = ((ELJTreeControl*)_obj)->GetSelections(sel);
	
	if (selections)
	{
		for (int i = 0; i < result; i++)
			*(((wxTreeItemId**)selections)[i]) = sel[i];
	}
	return result;		
}
	
EWXWEXPORT(void, wxTreeCtrl_GetParent)(void* _obj, void* item, void* _item)
{
#if wxVERSION_NUMBER < 2400
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetParent(*((wxTreeItemId*)item));
#else
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetItemParent(*((wxTreeItemId*)item));
#endif
}
	
EWXWEXPORT(void, wxTreeCtrl_GetFirstChild)(void* _obj, void* item, void* cookie, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetFirstChild(*((wxTreeItemId*)item), *((long*)cookie));
}
	
EWXWEXPORT(void, wxTreeCtrl_GetNextChild)(void* _obj, void* item, void* cookie, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetNextChild(*((wxTreeItemId*)item), *((long*)cookie));
}
	
EWXWEXPORT(void, wxTreeCtrl_GetLastChild)(void* _obj, void* item, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetLastChild(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_GetNextSibling)(void* _obj, void* item, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetNextSibling(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(void, wxTreeCtrl_GetPrevSibling)(void* _obj, void* item, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetPrevSibling(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(void, wxTreeCtrl_GetFirstVisibleItem)(void* _obj, void* item, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetFirstVisibleItem();
}

EWXWEXPORT(void, wxTreeCtrl_GetNextVisible)(void* _obj, void* item, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetNextVisible(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_GetPrevVisible)(void* _obj, void* item, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->GetPrevVisible(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_AddRoot)(void* _obj, void* text, int image, int selectedImage, void* data, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->AddRoot((char*) text, image, selectedImage, new ELJTreeItemData(data));
}
	
EWXWEXPORT(void, wxTreeCtrl_PrependItem)(void* _obj, void* parent, void* text, int image, int selectedImage, void* data, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->PrependItem(*((wxTreeItemId*)parent), (char*)text, image, selectedImage, new ELJTreeItemData(data));
}
	
EWXWEXPORT(void, wxTreeCtrl_InsertItem)(void* _obj, void* parent, void* idPrevious, void* text, int image, int selectedImage, void* data, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->InsertItem(*((wxTreeItemId*)parent), *((wxTreeItemId*)idPrevious), (char*)text, image, selectedImage, new ELJTreeItemData(data));
}
	
EWXWEXPORT(void, wxTreeCtrl_InsertItemByIndex)(void* _obj, void* parent, int index, void* text, int image, int selectedImage, void* data, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->InsertItem(*((wxTreeItemId*)parent), index, (char*)text, image, selectedImage, new ELJTreeItemData(data));
}
	
EWXWEXPORT(void, wxTreeCtrl_AppendItem)(void* _obj, void* parent, void* text, int image, int selectedImage, void* data, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->AppendItem(*((wxTreeItemId*) parent), (char*)text, image, selectedImage, new ELJTreeItemData(data));
}
	
EWXWEXPORT(void, wxTreeCtrl_Delete)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->Delete(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_DeleteChildren)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->DeleteChildren(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_DeleteAllItems)(void* _obj)
{
	((ELJTreeControl*)_obj)->DeleteAllItems();
}
	
EWXWEXPORT(void, wxTreeCtrl_Expand)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->Expand(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_Collapse)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->Collapse(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_CollapseAndReset)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->CollapseAndReset(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_Toggle)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->Toggle(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_Unselect)(void* _obj)
{
	((ELJTreeControl*)_obj)->Unselect();
}
	
EWXWEXPORT(void, wxTreeCtrl_UnselectAll)(void* _obj)
{
	((ELJTreeControl*)_obj)->UnselectAll();
}
	
EWXWEXPORT(void, wxTreeCtrl_SelectItem)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->SelectItem(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_EnsureVisible)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->EnsureVisible(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_ScrollTo)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->ScrollTo(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_EditLabel)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->EditLabel(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void*, wxTreeCtrl_GetEditControl)(void* _obj)
{
#ifdef __WIN32__
	return (void*)((ELJTreeControl*)_obj)->GetEditControl();
#else
	return NULL;
#endif
}
	
EWXWEXPORT(void, wxTreeCtrl_EndEditLabel)(void* _obj, void* item, int discardChanges)
{
#ifdef __WIN32__
	((ELJTreeControl*)_obj)->EndEditLabel(*((wxTreeItemId*)item), discardChanges != 0);
#endif
}
	
EWXWEXPORT(int, wxTreeCtrl_OnCompareItems)(void* _obj, void* item1, void* item2)
{
	return ((ELJTreeControl*)_obj)->OnCompareItems(*((wxTreeItemId*)item1), *((wxTreeItemId*)item2));
}
	
EWXWEXPORT(void, wxTreeCtrl_SortChildren)(void* _obj, void* item)
{
	((ELJTreeControl*)_obj)->SortChildren(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void, wxTreeCtrl_HitTest)(void* _obj, int _x, int _y, void* flags, void* _item)
{
	(*(wxTreeItemId*)_item) = ((ELJTreeControl*)_obj)->HitTest(wxPoint(_x, _y), *((int*)flags));
}
	
EWXWEXPORT(int, wxTreeCtrl_GetBoundingRect)(void* _obj, void* item, int textOnly, void* _x, void* _y, void* _w, void* _h)
{
#ifdef __WIN32__
	wxRect rct;
	int result = ((ELJTreeControl*)_obj)->GetBoundingRect(*((wxTreeItemId*)item), rct, textOnly != 0);
	if (result)
	{
		*((int*)_x) = rct.x;
		*((int*)_y) = rct.y;
		*((int*)_w) = rct.width;
		*((int*)_h) = rct.height;
	}
	return result;
#else
	return 0;
#endif
}
	
}
