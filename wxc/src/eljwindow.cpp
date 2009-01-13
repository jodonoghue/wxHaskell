#include "wrapper.h"
#include "wx/tooltip.h"

extern "C"
{

EWXWEXPORT(void*,wxWindow_Create)(wxWindow* _prt,int _id,int _x,int _y,int _w,int _h,int _stl)
{
	return (void*)new wxWindow(_prt, (wxWindowID)_id, wxPoint(_x, _y), wxSize(_w, _h), (long)_stl);
}
	
EWXWEXPORT(int,wxWindow_Close)(wxWindow* _obj,int _force)
{
	return (int)_obj->Close(_force != 0);
}
	
EWXWEXPORT(int,wxWindow_Destroy)(wxWindow* _obj)
{
	return (int)_obj->Destroy();
}
	
EWXWEXPORT(void,wxWindow_ClearBackground)(wxWindow* _obj)
{
  _obj->ClearBackground();
}
	
EWXWEXPORT(void,wxWindow_Fit)(wxWindow* _obj)
{
	_obj->Fit();
}
	
EWXWEXPORT(void,wxWindow_DestroyChildren)(wxWindow* _obj)
{
	_obj->DestroyChildren();
}
	
EWXWEXPORT(int,wxWindow_IsBeingDeleted)(wxWindow* _obj)
{
	return (int)_obj->IsBeingDeleted();
}
	
EWXWEXPORT(void,wxWindow_SetLabel)(wxWindow* _obj,wxString* _title)
{
	_obj->SetLabel(*_title);
}
	
EWXWEXPORT(wxString*,wxWindow_GetLabel)(wxWindow* _obj)
{
	wxString *result = new wxString();
	*result = _obj->GetLabel();
	return result;
}
	
EWXWEXPORT(int,wxWindow_GetLabelEmpty)(wxWindow* _obj)
{
	return (int)_obj->GetLabel().IsEmpty();
}
	
EWXWEXPORT(void,wxWindow_SetName)(wxWindow* _obj,wxString* _name)
{
	_obj->SetName(*_name);
}
	
EWXWEXPORT(wxString*,wxWindow_GetName)(wxWindow* _obj)
{
	wxString *result = new wxString();
	*result = _obj->GetName();
	return result;
}
	
EWXWEXPORT(void,wxWindow_SetId)(wxWindow* _obj,int _id)
{
	_obj->SetId(_id);
}
	
EWXWEXPORT(int,wxWindow_GetId)(wxWindow* _obj)
{
	return _obj->GetId();
}
	
EWXWEXPORT(void,wxWindow_SetSize)(wxWindow* _obj,int x,int y,int width,int height,int sizeFlags)
{
	_obj->SetSize(x, y, width, height, sizeFlags);
}
	
EWXWEXPORT(void,wxWindow_Move)(wxWindow* _obj,int x,int y)
{
	_obj->Move( x, y );
}
	
EWXWEXPORT(void,wxWindow_Raise)(wxWindow* _obj)
{
	_obj->Raise();
}
	
EWXWEXPORT(void,wxWindow_Lower)(wxWindow* _obj)
{
	_obj->Lower();
}
	
EWXWEXPORT(void,wxWindow_SetClientSize)(wxWindow* _obj,int width,int height)
{
	_obj->SetClientSize( width, height );
}
	
EWXWEXPORT(void, wxWindow_GetPosition)(void* _obj, void* _x, void* _y)
{
	((wxWindow*)_obj)->GetPosition((int*)_x, (int*)_y);
}
	
EWXWEXPORT(void, wxWindow_GetSize)(void* _obj, void* _w, void* _h)
{
	((wxWindow*)_obj)->GetSize((int*)_w, (int*)_h);
}
	
EWXWEXPORT(void, wxWindow_GetRect)(void* _obj, int* x, int* y, int* w, int* h)
{
	wxRect rc = ((wxWindow*)_obj)->GetRect();
	*x = rc.x;
	*y = rc.y;
	*w = rc.width;
	*h = rc.height;
}
	
EWXWEXPORT(void, wxWindow_GetClientSize)(void* _obj, void* _w, void* _h)
{
	((wxWindow*)_obj)->GetClientSize((int*)_w, (int*)_h);
}
	
EWXWEXPORT(void, wxWindow_GetBestSize)(void* _obj, void* _w, void* _h)
{
	((wxWindow*)_obj)->GetBestSize((int*)_w, (int*)_h);
}
	
EWXWEXPORT(void,wxWindow_Center)(wxWindow* _obj,int direction)
{
	_obj->Center( direction );
}
	
EWXWEXPORT(void,wxWindow_CenterOnParent)(wxWindow* _obj,int dir)
{
	_obj->CenterOnParent(dir);
}
	
EWXWEXPORT(void,wxWindow_SetSizeHints)(wxWindow* _obj,int minW,int minH,int maxW,int maxH,int incW,int incH)
{
	_obj->SetSizeHints( minW, minH, maxW, maxH, incW, incH );
}
	
EWXWEXPORT(int,wxWindow_GetMinWidth)(wxWindow* _obj)
{
	return _obj->GetMinWidth();
}
	
EWXWEXPORT(int,wxWindow_GetMinHeight)(wxWindow* _obj)
{
	return _obj->GetMinHeight();
}
	
EWXWEXPORT(int,wxWindow_GetMaxWidth)(wxWindow* _obj)
{
	return _obj->GetMaxWidth();
}

EWXWEXPORT(int,wxWindow_GetMaxHeight)(wxWindow* _obj)
{
	return _obj->GetMaxHeight();
}
	
EWXWEXPORT(int,wxWindow_Show)(wxWindow* _obj)
{
	return _obj->Show();
}
	
EWXWEXPORT(int,wxWindow_Hide)(wxWindow* _obj)
{
	return (int)_obj->Hide();
}
	
EWXWEXPORT(int,wxWindow_Enable)(wxWindow* _obj)
{
	return (int)_obj->Enable();
}

EWXWEXPORT(int,wxWindow_Disable)(wxWindow* _obj)
{
	return (int)_obj->Disable();
}
	
EWXWEXPORT(int,wxWindow_IsShown)(wxWindow* _obj)
{
	return (int)_obj->IsShown();
}
	
EWXWEXPORT(int,wxWindow_IsEnabled)(wxWindow* _obj)
{
	return (int)_obj->IsEnabled();
}
	
EWXWEXPORT(void,wxWindow_SetWindowStyleFlag)(wxWindow* _obj,long style)
{
	_obj->SetWindowStyleFlag( style );
}
	
EWXWEXPORT(int,wxWindow_GetWindowStyleFlag)(wxWindow* _obj)
{
	return (int)_obj->GetWindowStyleFlag();
}
	
EWXWEXPORT(int,wxWindow_HasFlag)(wxWindow* _obj,int flag)
{
	return (int)_obj->HasFlag(flag);
}
	
EWXWEXPORT(void,wxWindow_SetExtraStyle)(wxWindow* _obj,long exStyle)
{
	_obj->SetExtraStyle(exStyle);
}
	
EWXWEXPORT(void,wxWindow_MakeModal)(wxWindow* _obj,int modal)
{
	_obj->MakeModal(modal != 0);
}
	
EWXWEXPORT(void,wxWindow_SetFocus)(wxWindow* _obj)
{
	_obj->SetFocus();
}
	
EWXWEXPORT(void*,wxWindow_FindFocus)(wxWindow* _obj)
{
	return (void*)_obj->FindFocus();
}
	
EWXWEXPORT(int,wxWindow_GetChildren)(wxWindow* _obj,void* _res,int _cnt)
{
	if (_res && (unsigned int)_cnt == _obj->GetChildren().GetCount())
	{
		unsigned int i = 0;
		wxWindowList::compatibility_iterator node = _obj->GetChildren().GetFirst();;
		while (node)
		{
			((void**)_res)[i++] = (void*)(node->GetData());
			node = node->GetNext();
		}

		return i;
	}
	else
		return _obj->GetChildren().GetCount();
}
	
EWXWEXPORT(void*,wxWindow_GetParent)(wxWindow* _obj)
{
	return (void*)_obj->GetParent();
}
	
EWXWEXPORT(int,wxWindow_IsTopLevel)(wxWindow* _obj)
{
	return (int)_obj->IsTopLevel();
}
	
EWXWEXPORT(void*,wxWindow_FindWindow)(wxWindow* _obj,wxString* name)
{
	return (void*)_obj->FindWindow( *name );
}
	
EWXWEXPORT(void,wxWindow_AddChild)(wxWindow* _obj,void* child)
{
	_obj->AddChild( (wxWindowBase*) child );
}
	
EWXWEXPORT(void,wxWindow_RemoveChild)(wxWindow* _obj,void* child)
{
	_obj->RemoveChild( (wxWindowBase*) child );
}
	
EWXWEXPORT(void*,wxWindow_GetEventHandler)(wxWindow* _obj)
{
	return (void*)_obj->GetEventHandler();
}
	
EWXWEXPORT(void,wxWindow_PushEventHandler)(wxWindow* _obj,void* handler)
{
	_obj->PushEventHandler( (wxEvtHandler*) handler );
}
	
EWXWEXPORT(void*,wxWindow_PopEventHandler)(wxWindow* _obj,int deleteHandler)
{
	return (void*)_obj->PopEventHandler(deleteHandler != 0);
}
	
EWXWEXPORT(void,wxWindow_SetValidator)(wxWindow* _obj,void* validator)
{
	_obj->SetValidator(*((wxValidator*)validator));
}
	
EWXWEXPORT(void*,wxWindow_GetValidator)(wxWindow* _obj)
{
	return (void*)_obj->GetValidator();
}
	
EWXWEXPORT(void,wxWindow_SetClientData)(wxWindow* _obj,void* data)
{
	_obj->SetClientData( data );
}
	
EWXWEXPORT(void*,wxWindow_GetClientData)(wxWindow* _obj)
{
	return (void*) _obj->GetClientData();
}
	
EWXWEXPORT(int,wxWindow_Validate)(wxWindow* _obj)
{
	return (int)_obj->Validate();
}
	
EWXWEXPORT(int,wxWindow_TransferDataToWindow)(wxWindow* _obj)
{
	return (int)_obj->TransferDataToWindow();
}
	
EWXWEXPORT(int,wxWindow_TransferDataFromWindow)(wxWindow* _obj)
{
	return (int)_obj->TransferDataFromWindow();
}
	
EWXWEXPORT(void,wxWindow_InitDialog)(wxWindow* _obj)
{
	_obj->InitDialog();
}
	
EWXWEXPORT(void,wxWindow_SetAcceleratorTable)(wxWindow* _obj,void* accel)
{
	_obj->SetAcceleratorTable(*((wxAcceleratorTable*) accel));
}
	
EWXWEXPORT(void, wxWindow_ConvertPixelsToDialog)(void* _obj, int* x, int* y)
{
	wxPoint pt = ((wxWindow*)_obj)->ConvertPixelsToDialog(wxPoint(*x, *y));
	*x = pt.x;
	*y = pt.y;
}
	
EWXWEXPORT(void, wxWindow_ConvertDialogToPixels)(void* _obj, int* x, int* y)
{
	wxPoint pt = ((wxWindow*)_obj)->ConvertDialogToPixels(wxPoint(*x, *y));
	*x = pt.x;
	*y = pt.y;
}
	
EWXWEXPORT(void,wxWindow_WarpPointer)(wxWindow* _obj,int x,int y)
{
	_obj->WarpPointer(x, y);
}
	
EWXWEXPORT(void,wxWindow_CaptureMouse)(wxWindow* _obj)
{
	_obj->CaptureMouse();
}
	
EWXWEXPORT(void,wxWindow_ReleaseMouse)(wxWindow* _obj)
{
	_obj->ReleaseMouse();
}
	
EWXWEXPORT(void,wxWindow_Refresh)(wxWindow* _obj,int eraseBackground)
{
	_obj->Refresh(eraseBackground != 0, (const wxRect*) NULL);
}
	
EWXWEXPORT(void,wxWindow_RefreshRect)(wxWindow* _obj,int eraseBackground,int x,int y,int w,int h)
{
	const wxRect rect(x, y, w, h);
	_obj->Refresh(eraseBackground != 0, &rect);
}
	
EWXWEXPORT(void,wxWindow_PrepareDC)(wxWindow* _obj,void* dc)
{
	_obj->PrepareDC(*((wxDC*)dc));
}
	
EWXWEXPORT(void*,wxWindow_GetUpdateRegion)(wxWindow* _obj)
{
	return (void*) (&_obj->GetUpdateRegion());
}
	
EWXWEXPORT(int,wxWindow_IsExposed)(wxWindow* _obj,int x,int y,int w,int h)
{
	return (int)_obj->IsExposed( x, y, w, h );
}
	
EWXWEXPORT(int,wxWindow_SetBackgroundColour)(wxWindow* _obj,wxColour* colour)
{
	return (int)_obj->SetBackgroundColour(*colour);
}
	
EWXWEXPORT(void,wxWindow_SetForegroundColour)(wxWindow* _obj,wxColour* colour)
{
	_obj->SetForegroundColour(*colour);
}
	
EWXWEXPORT(void,wxWindow_GetBackgroundColour)(wxWindow* _obj,wxColour* colour)
{
	*colour = _obj->GetBackgroundColour();
}
	
EWXWEXPORT(void,wxWindow_GetForegroundColour)(wxWindow* _obj,wxColour* colour)
{
	*colour = _obj->GetForegroundColour();
}
	
EWXWEXPORT(void,wxWindow_SetCursor)(wxWindow* _obj,wxCursor* cursor)
{
	_obj->SetCursor(*cursor);
}
	
EWXWEXPORT(void*,wxWindow_GetCursor)(wxWindow* _obj)
{
	return (void*) (&_obj->GetCursor());
}
	
EWXWEXPORT(void,wxWindow_SetFont)(wxWindow* _obj,wxFont* font)
{
	_obj->SetFont(*font);
}
	
EWXWEXPORT(void,wxWindow_GetFont)(wxWindow* _obj,void* _font)
{
	(*(wxFont*)_font) = _obj->GetFont();
}
	
EWXWEXPORT(void,wxWindow_SetCaret)(wxWindow* _obj,void* caret)
{
	_obj->SetCaret((wxCaret*)caret);
}
	
EWXWEXPORT(void*,wxWindow_GetCaret)(wxWindow* _obj)
{
	return (void*)_obj->GetCaret();
}
	
EWXWEXPORT(int,wxWindow_GetCharHeight)(wxWindow* _obj)
{
	return _obj->GetCharHeight();
}
	
EWXWEXPORT(int,wxWindow_GetCharWidth)(wxWindow* _obj)
{
	return _obj->GetCharWidth();
}
	
EWXWEXPORT(void,wxWindow_GetTextExtent)(wxWindow* _obj,wxString* string,int* x,int* y,int* descent,int* externalLeading,void* theFont)
{
	_obj->GetTextExtent(*string, x,  y, descent, externalLeading, (const wxFont*) theFont );
}
	
EWXWEXPORT(void, wxWindow_ScreenToClient)(void* _obj, int *x, int *y)
{
	((wxWindow*)_obj)->ScreenToClient(x, y);
}
	
EWXWEXPORT(void,wxWindow_UpdateWindowUI)(wxWindow* _obj)
{
	_obj->UpdateWindowUI();
}
	
EWXWEXPORT(int,wxWindow_PopupMenu)(wxWindow* _obj,wxMenu* menu,int x,int y)
{
	return (int)_obj->PopupMenu(menu, x, y );
}
	
EWXWEXPORT(void,wxWindow_SetScrollPos)(wxWindow* _obj,int orient,int pos,int refresh)
{
	_obj->SetScrollPos( orient, pos, refresh != 0);
}
	
EWXWEXPORT(int,wxWindow_GetScrollPos)(wxWindow* _obj,int orient)
{
	return _obj->GetScrollPos( orient );
}
	
EWXWEXPORT(int,wxWindow_GetScrollThumb)(wxWindow* _obj,int orient)
{
	return _obj->GetScrollThumb( orient );
}
	
EWXWEXPORT(int,wxWindow_GetScrollRange)(wxWindow* _obj,int orient)
{
	return _obj->GetScrollRange( orient );
}
	
EWXWEXPORT(void,wxWindow_ScrollWindow)(wxWindow* _obj,int dx,int dy)
{
	_obj->ScrollWindow(dx, dy, (const wxRect*) NULL);
}
	
EWXWEXPORT(void,wxWindow_ScrollWindowRect)(wxWindow* _obj,int dx,int dy,int x,int y,int w,int h)
{
	const wxRect rect(x, y, w, h);
	_obj->ScrollWindow(dx, dy, &rect);
}
	
EWXWEXPORT(void,wxWindow_SetToolTip)(wxWindow* _obj,wxString* tip)
{
	_obj->SetToolTip( *tip );
}
	
EWXWEXPORT(int,wxWindow_GetToolTip)(wxWindow* _obj,void* _buf)
{
	wxToolTip* tip = _obj->GetToolTip();

	if (tip)
	{
                wxString res = tip->GetTip();
		return copyStrToBuf(_buf, res);
	}
	return 0;
}
	
EWXWEXPORT(void,wxWindow_SetDropTarget)(wxWindow* _obj,void* dropTarget)
{
	_obj->SetDropTarget((wxDropTarget*) dropTarget);
}
	
EWXWEXPORT(void*,wxWindow_GetDropTarget)(wxWindow* _obj)
{
	return (void*)_obj->GetDropTarget();
}
	
EWXWEXPORT(void,wxWindow_SetConstraints)(wxWindow* _obj,void* constraints)
{
	_obj->SetConstraints((wxLayoutConstraints*) constraints);
}
	
EWXWEXPORT(void*,wxWindow_GetConstraints)(wxWindow* _obj)
{
	return (void*)_obj->GetConstraints();
}
	
EWXWEXPORT(void,wxWindow_SetAutoLayout)(wxWindow* _obj,int autoLayout)
{
	_obj->SetAutoLayout( autoLayout != 0 );
}
	
EWXWEXPORT(int,wxWindow_GetAutoLayout)(wxWindow* _obj)
{
	return (int)_obj->GetAutoLayout();
}
	
EWXWEXPORT(void,wxWindow_Layout)(wxWindow* _obj)
{
	_obj->Layout();
}
	
EWXWEXPORT(void,wxWindow_UnsetConstraints)(wxWindow* _obj,void* c)
{
	_obj->UnsetConstraints((wxLayoutConstraints*) c);
}
	
EWXWEXPORT(void*,wxWindow_GetConstraintsInvolvedIn)(wxWindow* _obj)
{
	return (void*)_obj->GetConstraintsInvolvedIn();
}
	
EWXWEXPORT(void,wxWindow_AddConstraintReference)(wxWindow* _obj,void* otherWin)
{
	_obj->AddConstraintReference((wxWindowBase*) otherWin);
}
	
EWXWEXPORT(void,wxWindow_RemoveConstraintReference)(wxWindow* _obj,void* otherWin)
{
	_obj->RemoveConstraintReference((wxWindowBase*) otherWin);
}
	
EWXWEXPORT(void,wxWindow_DeleteRelatedConstraints)(wxWindow* _obj)
{
	_obj->DeleteRelatedConstraints();
}
	
EWXWEXPORT(void,wxWindow_ResetConstraints)(wxWindow* _obj)
{
	_obj->ResetConstraints();
}
	
EWXWEXPORT(void,wxWindow_SetConstraintSizes)(wxWindow* _obj,int recurse)
{
	_obj->SetConstraintSizes(recurse != 0);
}
	
EWXWEXPORT(int,wxWindow_LayoutPhase1)(wxWindow* _obj,int* noChanges)
{
	return (int)_obj->LayoutPhase1(noChanges);
}
	
EWXWEXPORT(int,wxWindow_LayoutPhase2)(wxWindow* _obj,int* noChanges)
{
	return (int)_obj->LayoutPhase2(noChanges);
}
	
EWXWEXPORT(int,wxWindow_DoPhase)(wxWindow* _obj,int phase)
{
	return (int)_obj->DoPhase(phase);
}
	
EWXWEXPORT(void,wxWindow_SetSizeConstraint)(wxWindow* _obj,int x,int y,int w,int h)
{
	_obj->SetSizeConstraint(x, y, w, h);
}
	
EWXWEXPORT(void,wxWindow_MoveConstraint)(wxWindow* _obj,int x,int y)
{
	_obj->MoveConstraint(x, y);
}
	
EWXWEXPORT(void,wxWindow_GetSizeConstraint)(wxWindow* _obj,int* w,int* h)
{
	_obj->GetSizeConstraint(w, h);
}
	
EWXWEXPORT(void,wxWindow_GetClientSizeConstraint)(wxWindow* _obj,int* w,int* h)
{
	_obj->GetClientSizeConstraint(w, h);
}
	
EWXWEXPORT(void,wxWindow_GetPositionConstraint)(wxWindow* _obj,int* x,int* y)
{
	_obj->GetPositionConstraint(x, y);
}
	
EWXWEXPORT(void,wxWindow_SetSizer)(wxWindow* _obj,void* sizer)
{
	_obj->SetSizer( (wxSizer*) sizer );
}
	
EWXWEXPORT(void*,wxWindow_GetSizer)(wxWindow* _obj)
{
	return (void*)_obj->GetSizer();
}
	
EWXWEXPORT(void*,wxWindow_GetHandle)(wxWindow* _obj)
{
	return (void*)_obj->GetHandle();
}
	
EWXWEXPORT(void,wxWindow_SetScrollbar)(wxWindow* _obj,int orient,int pos,int thumbVisible,int range,int refresh)
{
	_obj->SetScrollbar(orient, pos, thumbVisible, range, refresh != 0);
}

EWXWEXPORT(int,wxWindow_Reparent)(wxWindow* _obj,wxWindow* _par)
{
	return (int)_obj->Reparent(_par);
}

#if (wxVERSION_NUMBER < 2800)
EWXWEXPORT(void, wxWindow_GetAdjustedBestSize)(void* _obj, void* _w, void* _h)
{
	wxSize sz = ((wxWindow*)_obj)->GetAdjustedBestSize();
        *((int*)_w) = sz.GetWidth();
        *((int*)_h) = sz.GetHeight();
}
#else
EWXWEXPORT(void, wxWindow_GetEffectiveMinSize)(void* _obj, void* _w, void* _h)
{
	wxSize sz = ((wxWindow*)_obj)->GetEffectiveMinSize();
        *((int*)_w) = sz.GetWidth();
        *((int*)_h) = sz.GetHeight();
}
#endif

EWXWEXPORT(void,wxWindow_Freeze)(wxWindow* _obj)
{
	_obj->Freeze();
}

EWXWEXPORT(void,wxWindow_Thaw)(wxWindow* _obj)
{
	_obj->Thaw();
}

#if (wxVERSION_NUMBER >= 2800)
EWXWEXPORT(void, wxWindow_ClientToScreen)(void *_obj, int x, int y, int* sx, int* sy)
{
  wxPoint pt = ((wxWindow *)_obj)->ClientToScreen( wxPoint(x,y) );
  if (sx) *sx = pt.x;
  if (sy) *sy = pt.y;
}

EWXWEXPORT(void,wxWindow_FitInside)(wxWindow* _obj)
{
    _obj->FitInside();
}

EWXWEXPORT(void, wxWindow_SetVirtualSize)(void* _obj, int w, int h )
{
    ((wxWindow*)_obj)->SetVirtualSize( w, h );
}

EWXWEXPORT(void, wxWindow_GetVirtualSize)(void* _obj, int* w, int* h )
{
    ((wxWindow*)_obj)->GetVirtualSize( w, h );
}


#endif
}
