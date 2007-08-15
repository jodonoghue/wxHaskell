#include "wrapper.h"
#include <wx/dragimag.h>
#include <wx/generic/dragimgg.h>

extern "C" {
/*-----------------------------------------------------------------------------
  DragImage
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxDragImage*,wxDragImage_Create)( const wxBitmap* image, int x, int y )
{
  return new wxDragImage(*image, wxNullCursor, wxPoint(x, y));
}

EWXWEXPORT(wxDragImage*,wxDragIcon)( const wxIcon* icon, int x, int y )
{
  return new wxDragImage(*icon, wxNullCursor, wxPoint(x, y));
}

EWXWEXPORT(wxDragImage*,wxDragString)( const wxString* text, int x, int y )
{
  return new wxDragImage(*text, wxNullCursor, wxPoint(x, y));
}

EWXWEXPORT(wxDragImage*,wxDragTreeItem)( const wxTreeCtrl* treeCtrl, wxTreeItemId* id )
{
  return new wxDragImage(*treeCtrl, *id);
}

EWXWEXPORT(wxDragImage*,wxDragListItem)( const wxListCtrl* listCtrl, long id )
{
  return new wxDragImage(*listCtrl, id);
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragImage_Create)( const wxCursor* cursor, int x, int y )
{
  return new wxGenericDragImage(*cursor, wxPoint(x, y));
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragIcon)( const wxIcon* icon, int x, int y )
{
  return new wxGenericDragImage(*icon, wxNullCursor, wxPoint(x, y));
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragString)( const wxString* text, int x, int y )
{
  return new wxGenericDragImage(*text, wxNullCursor, wxPoint(x, y));
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragTreeItem)( const wxTreeCtrl* treeCtrl, wxTreeItemId* id )
{
  return new wxGenericDragImage(*treeCtrl, *id);
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragListItem)( const wxListCtrl* listCtrl, long id )
{
  return new wxGenericDragImage(*listCtrl, id);
}

EWXWEXPORT(void,wxDragImage_Delete)(wxDragImage* self)  
{
  if (self) delete self;
}

EWXWEXPORT(bool,wxDragImage_BeginDragFullScreen)( wxDragImage* self, int x_pos, int y_pos,
                                                  wxWindow* window, bool fullScreen,
                                                  wxRect* rect)
{
  return self->BeginDrag(wxPoint(x_pos, y_pos), window, fullScreen, rect);
}

EWXWEXPORT(bool,wxDragImage_BeginDrag)( wxDragImage* self, int x, int y,
                                        wxWindow* window, wxWindow* boundingWindow)
{
  return self->BeginDrag(wxPoint(x, y), window, boundingWindow);
}

EWXWEXPORT(bool,wxGenericDragImage_DoDrawImage)( wxGenericDragImage* self, wxDC* dc, int x, int y)
{
  return self->DoDrawImage(*dc, wxPoint(x, y));
}

EWXWEXPORT(bool,wxDragImage_EndDrag)( wxDragImage* self)
{
  return self->EndDrag();
}

EWXWEXPORT(void,wxGenericDragImage_GetImageRect)( wxGenericDragImage* self, int x_pos, int y_pos,
                                                  int* x, int* y, int* w, int* h)
{
  wxRect rc = self->GetImageRect(wxPoint(x_pos, y_pos));
  *x = rc.x;
  *y = rc.y;
  *w = rc.width;
  *h = rc.height;
}

EWXWEXPORT(bool,wxDragImage_Hide)( wxDragImage* self)
{
  return self->Hide();
}

EWXWEXPORT(bool,wxDragImage_Move)( wxDragImage* self, int x, int y)
{
  return self->Move(wxPoint(x, y));
}

EWXWEXPORT(bool,wxDragImage_Show)( wxDragImage* self)
{
  return self->Show();
}

EWXWEXPORT(bool,wxGenericDragImage_UpdateBackingFromWindow)( wxGenericDragImage* self, wxDC* windowDC, wxMemoryDC* destDC,
                                                             int x, int y, int w, int h,
                                                             int xdest, int ydest, int width, int height)
{
  return self->UpdateBackingFromWindow(*windowDC, *destDC,
                                       wxRect(x, y, w, h),
                                       wxRect(xdest, ydest, width, height));
}

}


