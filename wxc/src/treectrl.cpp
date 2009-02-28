#include "wrapper.h"

class wxcTreeItemData : public wxTreeItemData
{
  private:
    wxClosure* m_closure;

  public:
    wxcTreeItemData( wxClosure* closure ) { 
      m_closure = closure;
    }

    ~wxcTreeItemData() {
      if (m_closure) delete m_closure;
    }

    wxClosure* GetClientClosure() { 
      return m_closure; 
    }

    void SetClientClosure( wxClosure* closure ) {
      if (m_closure) delete m_closure;
      m_closure = closure;
    }
};

extern "C"
{

EWXWEXPORT(wxcTreeItemData*,wxcTreeItemData_Create)(wxClosure* closure)
{
	return new wxcTreeItemData(closure);
}

EWXWEXPORT(wxClosure*,wxcTreeItemData_GetClientClosure)(wxcTreeItemData* self)
{
	return self->GetClientClosure();
}

EWXWEXPORT(void,wxcTreeItemData_SetClientClosure)(wxcTreeItemData* self,wxClosure* closure)
{
	self->SetClientClosure(closure);
}

EWXWEXPORT(void*,wxTreeItemId_Create)()
{
	return new wxTreeItemId();
}

EWXWEXPORT(void,wxTreeItemId_Delete)(void* _obj)
{
	delete (wxTreeItemId*)_obj;
}

EWXWEXPORT(bool,wxTreeItemId_IsOk)(wxTreeItemId* _obj)
{
	return _obj->IsOk();
}

EWXWEXPORT(wxTreeItemId*,wxTreeItemId_Clone)(wxTreeItemId* _obj)
{
	wxTreeItemId* clone = new wxTreeItemId();
	*clone = *_obj;
	return clone;
}

// FIXME: wxHaskell uses this function in Graphics.UI.WXCore.WxcTypes.withTreeItemIdPtr
// to make wxTreeItemId.
//
// But wxWidgets' document says: wxTreemItemIds are not meant to be constructed
// explicitly by the user; they are returned by the wxTreeCtrl functions instead.
//
// http://www.wxwindows.org/manuals/2.8/wx_wxtreeitemid.html#wxtreeitemid
//
// So we must remove this function and replace treeItemId implementation in the
// funture.
EWXWEXPORT(wxTreeItemId*,wxTreeItemId_CreateFromValue)(int value)
{
#if wxVERSION_NUMBER < 2800
    return new wxTreeItemId( value );
#else
    // TODO: This function should be removed. No longer any equivalent in wxWidgets
    wxTreeItemId *item = new wxTreeItemId();
    item->m_pItem = reinterpret_cast<wxTreeItemIdValue>(value);
    return item;
#endif
}

EWXWEXPORT(int,wxTreeItemId_GetValue)(wxTreeItemId* _obj)
{
    return (long)(_obj->m_pItem);
}


EWXWEXPORT(wxKeyEvent*,wxTreeEvent_GetKeyEvent)(wxTreeEvent* _obj)
{
	return (wxKeyEvent*)&(_obj->GetKeyEvent());
}

EWXWEXPORT(bool,wxTreeEvent_IsEditCancelled)(wxTreeEvent* _obj)
{
	return _obj->IsEditCancelled();
}

EWXWEXPORT(void,wxTreeEvent_Allow)(wxTreeEvent* _obj)
{
	_obj->Allow();
}


EWXWEXPORT(void*,wxTreeCtrl_Create)(void* _obj,void* _cmp,wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxTreeCtrl (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(void*,wxTreeCtrl_Create2)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxTreeCtrl (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(int,wxTreeCtrl_GetCount)(void* _obj)
{
	int result =(int)((wxTreeCtrl*)_obj)->GetCount();
#ifdef __WXGTK__
	wxTreeItemId t = ((wxTreeCtrl*)_obj)->GetRootItem();
	if (t.IsOk()) result++;
#endif
	return result;
}
	
EWXWEXPORT(int,wxTreeCtrl_GetIndent)(void* _obj)
{
	return ((wxTreeCtrl*)_obj)->GetIndent();
}
	
EWXWEXPORT(void,wxTreeCtrl_SetIndent)(void* _obj,int indent)
{
	((wxTreeCtrl*)_obj)->SetIndent(indent);
}
	
EWXWEXPORT(int,wxTreeCtrl_GetSpacing)(void* _obj)
{
	return (int)((wxTreeCtrl*)_obj)->GetSpacing();
}
	
EWXWEXPORT(void,wxTreeCtrl_SetSpacing)(void* _obj,int spacing)
{
	((wxTreeCtrl*)_obj)->SetSpacing(spacing);
}
	
EWXWEXPORT(void*,wxTreeCtrl_GetImageList)(void* _obj)
{
	return (void*)((wxTreeCtrl*)_obj)->GetImageList();
}
	
EWXWEXPORT(void*,wxTreeCtrl_GetStateImageList)(void* _obj)
{
	return (void*)((wxTreeCtrl*)_obj)->GetStateImageList();
}

EWXWEXPORT(void,wxTreeCtrl_AssignImageList)(wxTreeCtrl* _obj,wxImageList* imageList)
{
	_obj->AssignImageList(imageList);
}

EWXWEXPORT(void,wxTreeCtrl_AssignStateImageList)(wxTreeCtrl* _obj,wxImageList* imageList)
{
	_obj->AssignStateImageList(imageList);
}

/*
EWXWEXPORT(wxImageList*,wxTreeCtrl_GetButtonsImageList)(wxTreeCtrl* _obj)
{
	return _obj->GetButtonsImageList();
}

EWXWEXPORT(void,wxTreeCtrl_SetButtonsImageList)(wxTreeCtrl* _obj,wxImageList* imageList)
{
	_obj->SetButtonsImageList(imageList);
}

EWXWEXPORT(void,wxTreeCtrl_AssignButtonsImageList)(wxTreeCtrl* _obj,wxImageList* imageList)
{
	_obj->AssignButtonsImageList(imageList);
}
*/

EWXWEXPORT(void,wxTreeCtrl_SetImageList)(void* _obj,void* imageList)
{
	((wxTreeCtrl*)_obj)->SetImageList((wxImageList*) imageList);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetStateImageList)(void* _obj,void* imageList)
{
	((wxTreeCtrl*)_obj)->SetStateImageList((wxImageList*) imageList);
}
	
EWXWEXPORT(wxString*,wxTreeCtrl_GetItemText)(void* _obj,void* item)
{
	wxString *result = new wxString();
	*result = ((wxTreeCtrl*)_obj)->GetItemText(*(wxTreeItemId*)item);
	return result;
}
	
EWXWEXPORT(int,wxTreeCtrl_GetItemImage)(void* _obj,void* item,int which)
{
	return ((wxTreeCtrl*)_obj)->GetItemImage(*((wxTreeItemId*) item), (wxTreeItemIcon) which);
}
	
EWXWEXPORT(void*,wxTreeCtrl_GetItemData)(void* _obj,void* item)
{
	return ((wxcTreeItemData*)((wxTreeCtrl*)_obj)->GetItemData(*((wxTreeItemId*) item)))->GetClientClosure();
}

EWXWEXPORT(void*,wxTreeCtrl_GetItemClientClosure)(void* _obj,void* item)
{
	return ((wxcTreeItemData*)((wxTreeCtrl*)_obj)->GetItemData(*((wxTreeItemId*) item)))->GetClientClosure();
}

	
EWXWEXPORT(void,wxTreeCtrl_SetItemText)(void* _obj,void* item,wxString* text)
{
	((wxTreeCtrl*)_obj)->SetItemText(*((wxTreeItemId*) item), *text);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemImage)(void* _obj,void* item,int image,int which)
{
	((wxTreeCtrl*)_obj)->SetItemImage(*((wxTreeItemId*)item), image, (wxTreeItemIcon)which);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemData)(void* _obj,void* item,wxClosure* closure)
{
	((wxTreeCtrl*)_obj)->SetItemData(*((wxTreeItemId*) item), new wxcTreeItemData (closure));
}

EWXWEXPORT(void,wxTreeCtrl_SetItemClientClosure)(wxTreeCtrl* _obj,void* item,wxClosure* closure)
{
        wxTreeItemData* oldData = _obj->GetItemData(*((wxTreeItemId*) item));
        /* bit unsafe: might delete twice but it is definitely ok on MSW 2.4.1 */
        if (oldData) delete oldData;
	_obj->SetItemData(*((wxTreeItemId*) item), new wxcTreeItemData (closure));
}

	
EWXWEXPORT(void,wxTreeCtrl_SetItemHasChildren)(void* _obj,void* item,bool has)
{
	((wxTreeCtrl*)_obj)->SetItemHasChildren(*((wxTreeItemId*)item), has);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemBold)(void* _obj,void* item,bool bold)
{
	((wxTreeCtrl*)_obj)->SetItemBold(*((wxTreeItemId*) item), bold);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemDropHighlight)(void* _obj,void* item,bool highlight)
{
#ifdef __WIN32__
	((wxTreeCtrl*)_obj)->SetItemDropHighlight(*((wxTreeItemId*) item), highlight);
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemTextColour)(void* _obj,void* item,void* col)
{
	((wxTreeCtrl*)_obj)->SetItemTextColour(*((wxTreeItemId*)item), *((wxColour*) col));
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemBackgroundColour)(void* _obj,void* item,void* col)
{
	((wxTreeCtrl*)_obj)->SetItemBackgroundColour(*((wxTreeItemId*) item), *((wxColour*) col));
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemFont)(void* _obj,void* item,void* font)
{
	((wxTreeCtrl*)_obj)->SetItemFont(*((wxTreeItemId*) item), *((wxFont*) font));
}
	
EWXWEXPORT(bool,wxTreeCtrl_IsVisible)(wxTreeCtrl* _obj,wxTreeItemId* item)
{
	return _obj->IsVisible(*item);
}
	
EWXWEXPORT(int,wxTreeCtrl_ItemHasChildren)(void* _obj,void* item)
{
	return (int)((wxTreeCtrl*)_obj)->ItemHasChildren(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(bool,wxTreeCtrl_IsExpanded)(wxTreeCtrl* _obj,wxTreeItemId* item)
{
	return _obj->IsExpanded(*item);
}
	
EWXWEXPORT(bool,wxTreeCtrl_IsSelected)(wxTreeCtrl* _obj,wxTreeItemId* item)
{
	return _obj->IsSelected(*item);
}
	
EWXWEXPORT(bool,wxTreeCtrl_IsBold)(wxTreeCtrl* _obj,wxTreeItemId* item)
{
	return _obj->IsBold(*item);
}
	
EWXWEXPORT(int,wxTreeCtrl_GetChildrenCount)(wxTreeCtrl* self,wxTreeItemId* item,bool recursively)
{
	return self->GetChildrenCount(* item, recursively);
}
	
EWXWEXPORT(void,wxTreeCtrl_GetRootItem)(void* _obj,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetRootItem();
}
	
EWXWEXPORT(void,wxTreeCtrl_GetSelection)(void* _obj,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetSelection();
}
	
EWXWEXPORT(int,wxTreeCtrl_GetSelections)(void* _obj,intptr_t* selections)
{
	int result = 0;
	wxArrayTreeItemIds sel;
	result = ((wxTreeCtrl*)_obj)->GetSelections(sel);
	
	if (selections)
	{
          for (int i = 0; i < result; i++) {
            /*
			*(((wxTreeItemId**)selections)[i]) = sel[i];
            */
#if (wxVERSION_NUMBER < 2800)
	    #if wxCHECK_VERSION(2,5,0)
            selections[i] = (intptr_t)(((wxTreeItemId*)sel[i])->m_pItem);
        #else
	        selections[i] = (intptr_t)(sel[i].m_pItem);
	    #endif
#else
            selections[i] = (intptr_t)(((wxTreeItemId)sel[i]).m_pItem);
#endif
	  }
	}
	return result;		
}
	
EWXWEXPORT(void,wxTreeCtrl_GetParent)(void* _obj,void* item,void* _item)
{
#if wxVERSION_NUMBER < 2400
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetParent(*((wxTreeItemId*)item));
#else
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetItemParent(*((wxTreeItemId*)item));
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_GetFirstChild)(void* _obj,void* item,void* cookie,void* _item)
{
#if wxVERSION_NUMBER < 2600
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetFirstChild(*((wxTreeItemId*)item), *((long*)cookie));
#else
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetFirstChild(*((wxTreeItemId*)item), cookie);
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_GetNextChild)(void* _obj,void* item,void* cookie,void* _item)
{
#if wxVERSION_NUMBER < 2600
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetNextChild(*((wxTreeItemId*)item), *((long*)cookie));
#else
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetNextChild(*((wxTreeItemId*)item), cookie);
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_GetLastChild)(void* _obj,void* item,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetLastChild(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_GetNextSibling)(void* _obj,void* item,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetNextSibling(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(void,wxTreeCtrl_GetPrevSibling)(void* _obj,void* item,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetPrevSibling(*((wxTreeItemId*) item));
}
	
EWXWEXPORT(void,wxTreeCtrl_GetFirstVisibleItem)(void* _obj,void* item,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetFirstVisibleItem();
}

EWXWEXPORT(void,wxTreeCtrl_GetNextVisible)(void* _obj,void* item,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetNextVisible(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_GetPrevVisible)(void* _obj,void* item,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->GetPrevVisible(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_AddRoot)(void* _obj,wxString* text,int image,int selectedImage,wxClosure* data,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->AddRoot(* text, image, selectedImage, new wxcTreeItemData(data));
}
	
EWXWEXPORT(void,wxTreeCtrl_PrependItem)(void* _obj,void* parent,wxString* text,int image,int selectedImage,wxClosure* data,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->PrependItem(*((wxTreeItemId*)parent), *text, image, selectedImage, new wxcTreeItemData(data));
}
	
EWXWEXPORT(void,wxTreeCtrl_InsertItem)(void* _obj,void* parent,void* idPrevious,wxString* text,int image,int selectedImage,wxClosure* data,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->InsertItem(*((wxTreeItemId*)parent), *((wxTreeItemId*)idPrevious), *text, image, selectedImage, new wxcTreeItemData(data));
}

EWXWEXPORT(void,wxTreeCtrl_InsertItem2)(void* _obj,void* parent,void* idPrevious,wxString* text,int image,int selectedImage,wxClosure* closure,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->InsertItem(*((wxTreeItemId*)parent), *((wxTreeItemId*)idPrevious), *text, image, selectedImage, new wxcTreeItemData(closure));
}

	
EWXWEXPORT(void,wxTreeCtrl_InsertItemByIndex)(void* _obj,void* parent,int index,wxString* text,int image,int selectedImage,wxClosure* data,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->InsertItem(*((wxTreeItemId*)parent), index, *text, image, selectedImage, new wxcTreeItemData(data));
}

EWXWEXPORT(void,wxTreeCtrl_InsertItemByIndex2)(void* _obj,void* parent,int index,wxString* text,int image,int selectedImage,wxClosure* data,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->InsertItem(*((wxTreeItemId*)parent), index, *text, image, selectedImage, new wxcTreeItemData(data));
}

	
EWXWEXPORT(void,wxTreeCtrl_AppendItem)(void* _obj,void* parent,wxString* text,int image,int selectedImage,wxClosure* data,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->AppendItem(*((wxTreeItemId*) parent), *text, image, selectedImage, new wxcTreeItemData(data));
}
	
EWXWEXPORT(void,wxTreeCtrl_Delete)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->Delete(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_DeleteChildren)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->DeleteChildren(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_DeleteAllItems)(void* _obj)
{
	((wxTreeCtrl*)_obj)->DeleteAllItems();
}
	
EWXWEXPORT(void,wxTreeCtrl_Expand)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->Expand(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_Collapse)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->Collapse(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_CollapseAndReset)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->CollapseAndReset(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_Toggle)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->Toggle(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_Unselect)(void* _obj)
{
	((wxTreeCtrl*)_obj)->Unselect();
}
	
EWXWEXPORT(void,wxTreeCtrl_UnselectAll)(void* _obj)
{
	((wxTreeCtrl*)_obj)->UnselectAll();
}
	
EWXWEXPORT(void,wxTreeCtrl_SelectItem)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->SelectItem(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_EnsureVisible)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->EnsureVisible(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_ScrollTo)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->ScrollTo(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_EditLabel)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->EditLabel(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void*,wxTreeCtrl_GetEditControl)(void* _obj)
{
#ifdef __WIN32__
	return (void*)((wxTreeCtrl*)_obj)->GetEditControl();
#else
	return NULL;
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_EndEditLabel)(void* _obj,void* item,bool discardChanges)
{
#ifdef __WIN32__
	((wxTreeCtrl*)_obj)->EndEditLabel(*((wxTreeItemId*)item), discardChanges);
#endif
}
	
EWXWEXPORT(int,wxTreeCtrl_OnCompareItems)(void* _obj,void* item1,void* item2)
{
	return ((wxTreeCtrl*)_obj)->OnCompareItems(*((wxTreeItemId*)item1), *((wxTreeItemId*)item2));
}
	
EWXWEXPORT(void,wxTreeCtrl_SortChildren)(void* _obj,void* item)
{
	((wxTreeCtrl*)_obj)->SortChildren(*((wxTreeItemId*)item));
}
	
EWXWEXPORT(void,wxTreeCtrl_HitTest)(void* _obj,int _x,int _y,void* flags,void* _item)
{
	(*(wxTreeItemId*)_item) = ((wxTreeCtrl*)_obj)->HitTest(wxPoint(_x, _y), *((int*)flags));
}
	
EWXWEXPORT(int,wxTreeCtrl_GetBoundingRect)(wxTreeCtrl* _obj,wxTreeItemId* item,bool textOnly,void* _x,void* _y,void* _w,void* _h)
{
#ifdef __WIN32__
	wxRect rct;
	int result = ((wxTreeCtrl*)_obj)->GetBoundingRect(*((wxTreeItemId*)item), rct, textOnly);
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
