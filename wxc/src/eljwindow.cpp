#include "wrapper.h"
#include "wx/tooltip.h"

extern "C"
{

EWXWEXPORT(void*, wxWindow_Create)(void* _prt, int _id, int _x, int _y, int _w, int _h, int _stl)
{
	return (void*)new wxWindow((wxWindow*)_prt, (wxWindowID)_id, wxPoint(_x, _y), wxSize(_w, _h), (long)_stl);
}
	
EWXWEXPORT(int, wxWindow_Close)(void* _obj, int _force)
{
	return (int)((wxWindow*)_obj)->Close(_force != 0);
}
	
EWXWEXPORT(int, wxWindow_Destroy)(void* _obj)
{
	return (int)((wxWindow*)_obj)->Destroy();
}
	
EWXWEXPORT(void, wxWindow_ClearBackground)(void* _obj)
{
  ((wxWindow*)_obj)->ClearBackground();
}
	
EWXWEXPORT(void, wxWindow_Fit)(void* _obj)
{
	((wxWindow*)_obj)->Fit();
}
	
EWXWEXPORT(int, wxWindow_DestroyChildren)(void* _obj)
{
	return (int)((wxWindow*)_obj)->DestroyChildren();
}
	
EWXWEXPORT(int, wxWindow_IsBeingDeleted)(void* _obj)
{
	return (int)((wxWindow*)_obj)->IsBeingDeleted();
}
	
EWXWEXPORT(void, wxWindow_SetLabel)(void* _obj, wxChar* _title)
{
	((wxWindow*)_obj)->SetLabel(_title);
}
	
EWXWEXPORT(int, wxWindow_GetLabel)(void* _obj, void* _buf)
{
	wxString result = ((wxWindow*)_obj)->GetLabel().c_str();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxWindow_GetLabelEmpty)(void* _obj)
{
	return (int)((wxWindow*)_obj)->GetLabel().IsEmpty();
}
	
EWXWEXPORT(void, wxWindow_SetName)(void* _obj, wxChar* _name)
{
	((wxWindow*)_obj)->SetName(_name);
}
	
EWXWEXPORT(int, wxWindow_GetName)(void* _obj, void* _buf)
{
	wxString result =((wxWindow*)_obj)->GetName();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(void, wxWindow_SetId)(void* _obj, int _id)
{
	((wxWindow*)_obj)->SetId(_id);
}
	
EWXWEXPORT(int, wxWindow_GetId)(void* _obj)
{
	return (int)((wxWindow*)_obj)->GetId();
}
	
EWXWEXPORT(void, wxWindow_SetSize)(void* _obj,  int x, int y, int width, int height, int sizeFlags)
{
	((wxWindow*)_obj)->SetSize(x, y, width, height, sizeFlags);
}
	
EWXWEXPORT(void, wxWindow_Move)(void* _obj,  int x, int y )
{
	((wxWindow*)_obj)->Move( x, y );
}
	
EWXWEXPORT(void, wxWindow_Raise)(void* _obj)
{
	((wxWindow*)_obj)->Raise();
}
	
EWXWEXPORT(void, wxWindow_Lower)(void* _obj)
{
	((wxWindow*)_obj)->Lower();
}
	
EWXWEXPORT(void, wxWindow_SetClientSize)(void* _obj, int width, int height )
{
	((wxWindow*)_obj)->SetClientSize( width, height );
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
	
EWXWEXPORT(void, wxWindow_Center)(void* _obj, int direction)
{
	((wxWindow*)_obj)->Center( direction );
}
	
EWXWEXPORT(void, wxWindow_CenterOnParent)(void* _obj, int dir)
{
	((wxWindow*)_obj)->CenterOnParent(dir);
}
	
EWXWEXPORT(void, wxWindow_SetSizeHints)(void* _obj, int minW, int minH,int maxW, int maxH, int incW, int incH)
{
	((wxWindow*)_obj)->SetSizeHints( minW, minH, maxW, maxH, incW, incH );
}
	
EWXWEXPORT(int, wxWindow_GetMinWidth)(void* _obj)
{
	return ((wxWindow*)_obj)->GetMinWidth();
}
	
EWXWEXPORT(int, wxWindow_GetMinHeight)(void* _obj)
{
	return ((wxWindow*)_obj)->GetMinHeight();
}
	
EWXWEXPORT(int, wxWindow_GetMaxWidth)(void* _obj)
{
	return ((wxWindow*)_obj)->GetMaxWidth();
}

EWXWEXPORT(int, wxWindow_GetMaxHeight)(void* _obj)
{
	return ((wxWindow*)_obj)->GetMaxHeight();
}
	
EWXWEXPORT(int, wxWindow_Show)(void* _obj)
{
	return ((wxWindow*)_obj)->Show();
}
	
EWXWEXPORT(int, wxWindow_Hide)(void* _obj)
{
	return (int)((wxWindow*)_obj)->Hide();
}
	
EWXWEXPORT(int, wxWindow_Enable)(void* _obj)
{
	return (int)((wxWindow*)_obj)->Enable();
}

EWXWEXPORT(int, wxWindow_Disable)(void* _obj)
{
	return (int)((wxWindow*)_obj)->Disable();
}
	
EWXWEXPORT(int, wxWindow_IsShown)(void* _obj)
{
	return (int)((wxWindow*)_obj)->IsShown();
}
	
EWXWEXPORT(int, wxWindow_IsEnabled)(void* _obj)
{
	return (int)((wxWindow*)_obj)->IsEnabled();
}
	
EWXWEXPORT(void, wxWindow_SetWindowStyleFlag)(void* _obj, long style)
{
	((wxWindow*)_obj)->SetWindowStyleFlag( style );
}
	
EWXWEXPORT(int, wxWindow_GetWindowStyleFlag)(void* _obj)
{
	return (int)((wxWindow*)_obj)->GetWindowStyleFlag();
}
	
EWXWEXPORT(int, wxWindow_HasFlag)(void* _obj, int flag)
{
	return (int)((wxWindow*)_obj)->HasFlag(flag);
}
	
EWXWEXPORT(void, wxWindow_SetExtraStyle)(void* _obj, long exStyle)
{
	((wxWindow*)_obj)->SetExtraStyle(exStyle);
}
	
EWXWEXPORT(void, wxWindow_MakeModal)(void* _obj, int modal)
{
	((wxWindow*)_obj)->MakeModal(modal != 0);
}
	
EWXWEXPORT(void, wxWindow_SetFocus)(void* _obj)
{
	((wxWindow*)_obj)->SetFocus();
}
	
EWXWEXPORT(void*, wxWindow_FindFocus)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->FindFocus();
}
	
EWXWEXPORT(int, wxWindow_GetChildren)(void* _obj, void* _res, int _cnt)
{
	if (_res && (unsigned int)_cnt == ((wxWindow*)_obj)->GetChildren().GetCount())
	{
		unsigned int i = 0;
		wxWindowList::compatibility_iterator node = 
			((wxWindow*)_obj)->GetChildren().GetFirst();
	
		while (node)
		{
			((void**)_res)[i++] = (void*)(node->GetData());
			node = node->GetNext();
		}

		return i;
	}
	else
		return ((wxWindow*)_obj)->GetChildren().GetCount();
}
	
EWXWEXPORT(void*, wxWindow_GetParent)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->GetParent();
}
	
EWXWEXPORT(int, wxWindow_IsTopLevel)(void* _obj)
{
	return (int)((wxWindow*)_obj)->IsTopLevel();
}
	
EWXWEXPORT(void*, wxWindow_FindWindow)(void* _obj, wxChar* name )
{
	return (void*)((wxWindow*)_obj)->FindWindow( name );
}
	
EWXWEXPORT(void, wxWindow_AddChild)(void* _obj, void* child )
{
	((wxWindow*)_obj)->AddChild( (wxWindowBase*) child );
}
	
EWXWEXPORT(void, wxWindow_RemoveChild)(void* _obj, void* child )
{
	((wxWindow*)_obj)->RemoveChild( (wxWindowBase*) child );
}
	
EWXWEXPORT(void*, wxWindow_GetEventHandler)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->GetEventHandler();
}
	
EWXWEXPORT(void, wxWindow_PushEventHandler)(void* _obj, void* handler )
{
	((wxWindow*)_obj)->PushEventHandler( (wxEvtHandler*) handler );
}
	
EWXWEXPORT(void*, wxWindow_PopEventHandler)(void* _obj, int deleteHandler)
{
	return (void*)((wxWindow*)_obj)->PopEventHandler(deleteHandler != 0);
}
	
EWXWEXPORT(void, wxWindow_SetValidator)(void* _obj, void* validator )
{
	((wxWindow*)_obj)->SetValidator(*((wxValidator*)validator));
}
	
EWXWEXPORT(void*, wxWindow_GetValidator)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->GetValidator();
}
	
EWXWEXPORT(void, wxWindow_SetClientData)(void* _obj, void *data )
{
	((wxWindow*)_obj)->SetClientData( data );
}
	
EWXWEXPORT(void*, wxWindow_GetClientData)(void* _obj)
{
	return (void*) ((wxWindow*)_obj)->GetClientData();
}
	
EWXWEXPORT(int, wxWindow_Validate)(void* _obj)
{
	return (int)((wxWindow*)_obj)->Validate();
}
	
EWXWEXPORT(int, wxWindow_TransferDataToWindow)(void* _obj)
{
	return (int)((wxWindow*)_obj)->TransferDataToWindow();
}
	
EWXWEXPORT(int, wxWindow_TransferDataFromWindow)(void* _obj)
{
	return (int)((wxWindow*)_obj)->TransferDataFromWindow();
}
	
EWXWEXPORT(void, wxWindow_InitDialog)(void* _obj)
{
	((wxWindow*)_obj)->InitDialog();
}
	
EWXWEXPORT(void, wxWindow_SetAcceleratorTable)(void* _obj, void* accel )
{
	((wxWindow*)_obj)->SetAcceleratorTable(*((wxAcceleratorTable*) accel));
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
	
EWXWEXPORT(void, wxWindow_WarpPointer)(void* _obj, int x, int y)
{
	((wxWindow*)_obj)->WarpPointer(x, y);
}
	
EWXWEXPORT(void, wxWindow_CaptureMouse)(void* _obj)
{
	((wxWindow*)_obj)->CaptureMouse();
}
	
EWXWEXPORT(void, wxWindow_ReleaseMouse)(void* _obj)
{
	((wxWindow*)_obj)->ReleaseMouse();
}
	
EWXWEXPORT(void, wxWindow_Refresh)(void* _obj, int eraseBackground)
{
	((wxWindow*)_obj)->Refresh(eraseBackground != 0, (const wxRect*) NULL);
}
	
EWXWEXPORT(void, wxWindow_RefreshRect)(void* _obj, int eraseBackground, int x, int y, int w, int h)
{
	const wxRect rect(x, y, w, h);
	((wxWindow*)_obj)->Refresh(eraseBackground != 0, &rect);
}
	
EWXWEXPORT(void, wxWindow_PrepareDC)(void* _obj, void* dc)
{
	((wxWindow*)_obj)->PrepareDC(*((wxDC*)dc));
}
	
EWXWEXPORT(void*, wxWindow_GetUpdateRegion)(void* _obj)
{
	return (void*) (&((wxWindow*)_obj)->GetUpdateRegion());
}
	
EWXWEXPORT(int, wxWindow_IsExposed)(void* _obj,  int x, int y, int w, int h )
{
	return (int)((wxWindow*)_obj)->IsExposed( x, y, w, h );
}
	
EWXWEXPORT(int, wxWindow_SetBackgroundColour)(void* _obj, void* colour)
{
	return (int)((wxWindow*)_obj)->SetBackgroundColour(*((wxColour*) colour));
}
	
EWXWEXPORT(int, wxWindow_SetForegroundColour)(void* _obj, void* colour)
{
	return (int)((wxWindow*)_obj)->SetForegroundColour(*((wxColour*) colour));
}
	
EWXWEXPORT(void, wxWindow_GetBackgroundColour)(void* _obj, void* colour)
{
	*((wxColour*)colour) = ((wxWindow*)_obj)->GetBackgroundColour();
}
	
EWXWEXPORT(void, wxWindow_GetForegroundColour)(void* _obj, void* colour)
{
	*((wxColour*)colour) = ((wxWindow*)_obj)->GetForegroundColour();
}
	
EWXWEXPORT(int, wxWindow_SetCursor)(void* _obj, void* cursor)
{
	return (int)((wxWindow*)_obj)->SetCursor(*((wxCursor*) cursor));
}
	
EWXWEXPORT(void*, wxWindow_GetCursor)(void* _obj)
{
	return (void*) (&((wxWindow*)_obj)->GetCursor());
}
	
EWXWEXPORT(int, wxWindow_SetFont)(void* _obj,  void* font)
{
	return (int)((wxWindow*)_obj)->SetFont( *((wxFont*)font) );
}
	
EWXWEXPORT(void, wxWindow_GetFont)(void* _obj, void* _font)
{
	(*(wxFont*)_font) = ((wxWindow*)_obj)->GetFont();
}
	
EWXWEXPORT(void, wxWindow_SetCaret)(void* _obj, void* caret)
{
	((wxWindow*)_obj)->SetCaret((wxCaret*)caret);
}
	
EWXWEXPORT(void*, wxWindow_GetCaret)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->GetCaret();
}
	
EWXWEXPORT(int, wxWindow_GetCharHeight)(void* _obj)
{
	return ((wxWindow*)_obj)->GetCharHeight();
}
	
EWXWEXPORT(int, wxWindow_GetCharWidth)(void* _obj)
{
	return ((wxWindow*)_obj)->GetCharWidth();
}
	
EWXWEXPORT(void, wxWindow_GetTextExtent)(void* _obj, wxChar* string, int* x, int* y, int* descent, int* externalLeading, void* theFont)
{
	((wxWindow*)_obj)->GetTextExtent(string, x,  y, descent, externalLeading, (const wxFont*) theFont );
}
	
EWXWEXPORT(void, wxWindow_ScreenToClient)(void* _obj, int *x, int *y)
{
	((wxWindow*)_obj)->ScreenToClient(x, y);
}
	
EWXWEXPORT(void, wxWindow_UpdateWindowUI)(void* _obj)
{
	((wxWindow*)_obj)->UpdateWindowUI();
}
	
EWXWEXPORT(int, wxWindow_PopupMenu)(void* _obj, void* menu, int x, int y )
{
	return (int)((wxWindow*)_obj)->PopupMenu((wxMenu*) menu, x, y );
}
	
EWXWEXPORT(void, wxWindow_SetScrollPos)(void* _obj, int orient, int pos, int refresh)
{
	((wxWindow*)_obj)->SetScrollPos( orient, pos, refresh != 0);
}
	
EWXWEXPORT(int, wxWindow_GetScrollPos)(void* _obj, int orient )
{
	return ((wxWindow*)_obj)->GetScrollPos( orient );
}
	
EWXWEXPORT(int, wxWindow_GetScrollThumb)(void* _obj, int orient )
{
	return ((wxWindow*)_obj)->GetScrollThumb( orient );
}
	
EWXWEXPORT(int, wxWindow_GetScrollRange)(void* _obj, int orient )
{
	return ((wxWindow*)_obj)->GetScrollRange( orient );
}
	
EWXWEXPORT(void, wxWindow_ScrollWindow)(void* _obj, int dx, int dy)
{
	((wxWindow*)_obj)->ScrollWindow(dx, dy, (const wxRect*) NULL);
}
	
EWXWEXPORT(void, wxWindow_ScrollWindowRect)(void* _obj, int dx, int dy, int x, int y, int w, int h)
{
	const wxRect rect(x, y, w, h);
	((wxWindow*)_obj)->ScrollWindow(dx, dy, &rect);
}
	
EWXWEXPORT(void, wxWindow_SetToolTip)(void* _obj, wxChar* tip )
{
	((wxWindow*)_obj)->SetToolTip( tip );
}
	
EWXWEXPORT(int, wxWindow_GetToolTip)(void* _obj, void* _buf)
{
	wxToolTip* tip = ((wxWindow*)_obj)->GetToolTip();

	if (tip)
	{
                wxString res = tip->GetTip();
		return copyStrToBuf(_buf, res);
	}
	return 0;
}
	
EWXWEXPORT(void, wxWindow_SetDropTarget)(void* _obj, void* dropTarget )
{
	((wxWindow*)_obj)->SetDropTarget((wxDropTarget*) dropTarget);
}
	
EWXWEXPORT(void*, wxWindow_GetDropTarget)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->GetDropTarget();
}
	
EWXWEXPORT(void, wxWindow_SetConstraints)(void* _obj, void* constraints )
{
	((wxWindow*)_obj)->SetConstraints((wxLayoutConstraints*) constraints);
}
	
EWXWEXPORT(void*, wxWindow_GetConstraints)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->GetConstraints();
}
	
EWXWEXPORT(void, wxWindow_SetAutoLayout)(void* _obj, int autoLayout )
{
	((wxWindow*)_obj)->SetAutoLayout( autoLayout != 0 );
}
	
EWXWEXPORT(int, wxWindow_GetAutoLayout)(void* _obj)
{
	return (int)((wxWindow*)_obj)->GetAutoLayout();
}
	
EWXWEXPORT(int, wxWindow_Layout)(void* _obj)
{
	return (int)((wxWindow*)_obj)->Layout();
}
	
EWXWEXPORT(void, wxWindow_UnsetConstraints)(void* _obj, void* c)
{
	((wxWindow*)_obj)->UnsetConstraints((wxLayoutConstraints*) c);
}
	
EWXWEXPORT(void*, wxWindow_GetConstraintsInvolvedIn)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->GetConstraintsInvolvedIn();
}
	
EWXWEXPORT(void, wxWindow_AddConstraintReference)(void* _obj, void* otherWin)
{
	((wxWindow*)_obj)->AddConstraintReference((wxWindowBase*) otherWin);
}
	
EWXWEXPORT(void, wxWindow_RemoveConstraintReference)(void* _obj, void* otherWin)
{
	((wxWindow*)_obj)->RemoveConstraintReference((wxWindowBase*) otherWin);
}
	
EWXWEXPORT(void, wxWindow_DeleteRelatedConstraints)(void* _obj)
{
	((wxWindow*)_obj)->DeleteRelatedConstraints();
}
	
EWXWEXPORT(void, wxWindow_ResetConstraints)(void* _obj)
{
	((wxWindow*)_obj)->ResetConstraints();
}
	
EWXWEXPORT(void, wxWindow_SetConstraintSizes)(void* _obj, int recurse)
{
	((wxWindow*)_obj)->SetConstraintSizes(recurse != 0);
}
	
EWXWEXPORT(int, wxWindow_LayoutPhase1)(void* _obj, int *noChanges)
{
	return (int)((wxWindow*)_obj)->LayoutPhase1(noChanges);
}
	
EWXWEXPORT(int, wxWindow_LayoutPhase2)(void* _obj, int *noChanges)
{
	return (int)((wxWindow*)_obj)->LayoutPhase2(noChanges);
}
	
EWXWEXPORT(int, wxWindow_DoPhase)(void* _obj, int phase)
{
	return (int)((wxWindow*)_obj)->DoPhase(phase);
}
	
EWXWEXPORT(void, wxWindow_SetSizeConstraint)(void* _obj, int x, int y, int w, int h)
{
	((wxWindow*)_obj)->SetSizeConstraint(x, y, w, h);
}
	
EWXWEXPORT(void, wxWindow_MoveConstraint)(void* _obj, int x, int y)
{
	((wxWindow*)_obj)->MoveConstraint(x, y);
}
	
EWXWEXPORT(void, wxWindow_GetSizeConstraint)(void* _obj, int *w, int *h)
{
	((wxWindow*)_obj)->GetSizeConstraint(w, h);
}
	
EWXWEXPORT(void, wxWindow_GetClientSizeConstraint)(void* _obj, int *w, int *h)
{
	((wxWindow*)_obj)->GetClientSizeConstraint(w, h);
}
	
EWXWEXPORT(void, wxWindow_GetPositionConstraint)(void* _obj, int *x, int *y)
{
	((wxWindow*)_obj)->GetPositionConstraint(x, y);
}
	
EWXWEXPORT(void, wxWindow_SetSizer)(void* _obj, void* sizer )
{
	((wxWindow*)_obj)->SetSizer( (wxSizer*) sizer );
}
	
EWXWEXPORT(void*, wxWindow_GetSizer)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->GetSizer();
}
	
EWXWEXPORT(void*, wxWindow_GetHandle)(void* _obj)
{
	return (void*)((wxWindow*)_obj)->GetHandle();
}
	
EWXWEXPORT(void, wxWindow_SetScrollbar)(void* _obj, int orient, int pos, int thumbVisible, int range, int refresh)
{
    ((wxWindow*)_obj)->SetScrollbar(orient, pos, thumbVisible, range, refresh != 0);
}

EWXWEXPORT(int, wxWindow_Reparent)(void* _obj, void* _par)
{
	return (int)((wxWindow*)_obj)->Reparent((wxWindow*)_par);
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

EWXWEXPORT(void, wxWindow_Freeze)(void* _obj)
{
	((wxWindow*)_obj)->Freeze();
}

EWXWEXPORT(void, wxWindow_Thaw)(void* _obj)
{
	((wxWindow*)_obj)->Thaw();
}

#if (wxVERSION_NUMBER >= 2800)
EWXWEXPORT(void, wxWindow_ClientToScreen)(void *_obj, int x, int y, int* sx, int* sy)
{
  wxPoint pt = ((wxWindow *)_obj)->ClientToScreen( wxPoint(x,y) );
  if (sx) *sx = pt.x;
  if (sy) *sy = pt.y;
}

EWXWEXPORT(void, wxWindow_FitInside)(void* _obj)
{
    ((wxWindow*)_obj)->FitInside();
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
