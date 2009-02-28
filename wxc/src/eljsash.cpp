#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxSashWindow_Create)(wxWindow* _par,int _id,int _x,int _y,int _w,int _h,int _stl)
{
	return (void*)new wxSashWindow (_par, _id, wxPoint(_x, _y), wxSize (_w, _h), (long)_stl);
}

EWXWEXPORT(void,wxSashWindow_SetSashVisible)(void* self,int edge,bool sash)
{
	((wxSashWindow*)self)->SetSashVisible((wxSashEdgePosition)edge, sash);
}
	
EWXWEXPORT(bool,wxSashWindow_GetSashVisible)(wxSashWindow* self,int edge)
{
	return self->GetSashVisible((wxSashEdgePosition)edge);
}
	
EWXWEXPORT(void,wxSashWindow_SetSashBorder)(wxSashWindow* self,int edge,bool border)
{
#if WXWIN_COMPATIBILITY_2_6
	self->SetSashBorder((wxSashEdgePosition)edge, border);
#endif
}
	
EWXWEXPORT(bool,wxSashWindow_HasBorder)(wxSashWindow* self,int edge)
{
#if WXWIN_COMPATIBILITY_2_6
	return self->HasBorder((wxSashEdgePosition)edge);
#else
	return false;
#endif
}
	
EWXWEXPORT(int, wxSashWindow_GetEdgeMargin)(void* _obj, int edge)
{
	return ((wxSashWindow*)_obj)->GetEdgeMargin((wxSashEdgePosition)edge);
}
	
EWXWEXPORT(void, wxSashWindow_SetDefaultBorderSize)(void* _obj, int width)
{
	((wxSashWindow*)_obj)->SetDefaultBorderSize(width);
}
	
EWXWEXPORT(int, wxSashWindow_GetDefaultBorderSize)(void* _obj)
{
	return ((wxSashWindow*)_obj)->GetDefaultBorderSize();
}
	
EWXWEXPORT(void, wxSashWindow_SetExtraBorderSize)(void* _obj, int width)
{
	((wxSashWindow*)_obj)->SetExtraBorderSize(width);
}
	
EWXWEXPORT(int, wxSashWindow_GetExtraBorderSize)(void* _obj)
{
	return ((wxSashWindow*)_obj)->GetExtraBorderSize();
}
	
EWXWEXPORT(void, wxSashWindow_SetMinimumSizeX)(void* _obj, int min)
{
	((wxSashWindow*)_obj)->SetMinimumSizeX(min);
}
	
EWXWEXPORT(void, wxSashWindow_SetMinimumSizeY)(void* _obj, int min)
{
	((wxSashWindow*)_obj)->SetMinimumSizeY(min);
}
	
EWXWEXPORT(int, wxSashWindow_GetMinimumSizeX)(void* _obj)
{
	return ((wxSashWindow*)_obj)->GetMinimumSizeX();
}
	
EWXWEXPORT(int, wxSashWindow_GetMinimumSizeY)(void* _obj)
{
	return ((wxSashWindow*)_obj)->GetMinimumSizeY();
}
	
EWXWEXPORT(void, wxSashWindow_SetMaximumSizeX)(void* _obj, int max)
{
	((wxSashWindow*)_obj)->SetMaximumSizeX(max);
}
	
EWXWEXPORT(void, wxSashWindow_SetMaximumSizeY)(void* _obj, int max)
{
	((wxSashWindow*)_obj)->SetMaximumSizeY(max);
}
	
EWXWEXPORT(int, wxSashWindow_GetMaximumSizeX)(void* _obj)
{
	return ((wxSashWindow*)_obj)->GetMaximumSizeX();
}
	
EWXWEXPORT(int, wxSashWindow_GetMaximumSizeY)(void* _obj)
{
	return ((wxSashWindow*)_obj)->GetMaximumSizeY();
}


EWXWEXPORT(void*, wxSashEvent_Create) (int id, int edge)
{
	return (void*) new wxSashEvent(id, (wxSashEdgePosition)edge);
}

EWXWEXPORT(void, wxSashEvent_SetEdge)(void* _obj, int edge)
{
	((wxSashEvent*)_obj)->SetEdge((wxSashEdgePosition)edge);
}
	
EWXWEXPORT(int, wxSashEvent_GetEdge)(void* _obj)
{
	return (int)((wxSashEvent*)_obj)->GetEdge();
}
	
EWXWEXPORT(void, wxSashEvent_SetDragRect)(void* _obj, int x, int y , int w, int h)
{
	((wxSashEvent*)_obj)->SetDragRect(wxRect (x, y , w, h));
}
	
EWXWEXPORT(void, wxSashEvent_GetDragRect)(void* _obj, void* x, void* y , void* w, void* h)
{
	wxRect tmp = ((wxSashEvent*)_obj)->GetDragRect();
	(*(int*)x) = tmp.x;
	(*(int*)y) = tmp.y;
	(*(int*)w) = tmp.width;
	(*(int*)h) = tmp.height;
}
	
EWXWEXPORT(void, wxSashEvent_SetDragStatus)(void* _obj, int status)
{
	((wxSashEvent*)_obj)->SetDragStatus((wxSashDragStatus)status);
}
	
EWXWEXPORT(int, wxSashEvent_GetDragStatus)(void* _obj)
{
	return (int)((wxSashEvent*)_obj)->GetDragStatus();
}
	

EWXWEXPORT(void*, wxSashLayoutWindow_Create) (void* _par, int _id, int _x, int _y, int _w, int _h, int _stl)
{
	return (void*) new wxSashLayoutWindow ((wxWindow*)_par, _id, wxPoint(_x, _y), wxSize (_w, _h), (long)_stl);
}

EWXWEXPORT(int, wxSashLayoutWindow_GetAlignment)(void* _obj)
{
	return (int)((wxSashLayoutWindow*)_obj)->GetAlignment();
}
	
EWXWEXPORT(int, wxSashLayoutWindow_GetOrientation)(void* _obj)
{
	return (int)((wxSashLayoutWindow*)_obj)->GetOrientation();
}
	
EWXWEXPORT(void, wxSashLayoutWindow_SetAlignment)(void* _obj, int align)
{
	((wxSashLayoutWindow*)_obj)->SetAlignment((wxLayoutAlignment)align);
}
	
EWXWEXPORT(void, wxSashLayoutWindow_SetOrientation)(void* _obj, int orient)
{
	((wxSashLayoutWindow*)_obj)->SetOrientation((wxLayoutOrientation)orient);
}
	
EWXWEXPORT(void, wxSashLayoutWindow_SetDefaultSize)(void* _obj, int w, int h)
{
	((wxSashLayoutWindow*)_obj)->SetDefaultSize(wxSize(w, h));
}
	

EWXWEXPORT(void*, wxQueryLayoutInfoEvent_Create) (int id)
{
	return (void*) new wxQueryLayoutInfoEvent(id);
}

EWXWEXPORT(void, wxQueryLayoutInfoEvent_SetRequestedLength)(void* _obj, int length)
{
	((wxQueryLayoutInfoEvent*)_obj)->SetRequestedLength(length);
}
	
EWXWEXPORT(int, wxQueryLayoutInfoEvent_GetRequestedLength)(void* _obj)
{
	return ((wxQueryLayoutInfoEvent*)_obj)->GetRequestedLength();
}
	
EWXWEXPORT(void, wxQueryLayoutInfoEvent_SetFlags)(void* _obj, int flags)
{
	((wxQueryLayoutInfoEvent*)_obj)->SetFlags(flags);
}
	
EWXWEXPORT(int, wxQueryLayoutInfoEvent_GetFlags)(void* _obj)
{
	return ((wxQueryLayoutInfoEvent*)_obj)->GetFlags();
}
	
EWXWEXPORT(void, wxQueryLayoutInfoEvent_SetSize)(void* _obj, int w, int h)
{
	((wxQueryLayoutInfoEvent*)_obj)->SetSize(wxSize(w, h));
}
	
EWXWEXPORT(void, wxQueryLayoutInfoEvent_GetSize)(void* _obj, void* w, void* h)
{
	wxSize tmp = ((wxQueryLayoutInfoEvent*)_obj)->GetSize();
	(*(int*)w) = tmp.x;
	(*(int*)h) = tmp.y;
}
	
EWXWEXPORT(void, wxQueryLayoutInfoEvent_SetOrientation)(void* _obj, int orient)
{
	((wxQueryLayoutInfoEvent*)_obj)->SetOrientation((wxLayoutOrientation)orient);
}
	
EWXWEXPORT(int, wxQueryLayoutInfoEvent_GetOrientation)(void* _obj)
{
	return (int)((wxQueryLayoutInfoEvent*)_obj)->GetOrientation();
}
	
EWXWEXPORT(void, wxQueryLayoutInfoEvent_SetAlignment)(void* _obj, int align)
{
	((wxQueryLayoutInfoEvent*)_obj)->SetAlignment((wxLayoutAlignment)align);
}
	
EWXWEXPORT(int, wxQueryLayoutInfoEvent_GetAlignment)(void* _obj)
{
	return (int)((wxQueryLayoutInfoEvent*)_obj)->GetAlignment();
}
	

EWXWEXPORT(void*, wxCalculateLayoutEvent_Create) (int id)
{
	return (void*) new wxCalculateLayoutEvent(id);
}

EWXWEXPORT(void, wxCalculateLayoutEvent_SetFlags)(void* _obj, int flags)
{
	((wxCalculateLayoutEvent*)_obj)->SetFlags(flags);
}
	
EWXWEXPORT(int, wxCalculateLayoutEvent_GetFlags)(void* _obj)
{
	return ((wxCalculateLayoutEvent*)_obj)->GetFlags();
}
	
EWXWEXPORT(void, wxCalculateLayoutEvent_SetRect)(void* _obj, int x, int y , int w, int h)
{
	((wxCalculateLayoutEvent*)_obj)->SetRect(wxRect(x, y, w, h));
}
	
EWXWEXPORT(void, wxCalculateLayoutEvent_GetRect)(void* _obj, void* x, void* y , void* w, void* h)
{
	wxRect tmp = ((wxCalculateLayoutEvent*)_obj)->GetRect();
	(*(int*)x) = tmp.x;
	(*(int*)y) = tmp.y;
	(*(int*)w) = tmp.width;
	(*(int*)h) = tmp.height;
}

EWXWEXPORT(void*, wxLayoutAlgorithm_Create)()
{
	return (void*) new wxLayoutAlgorithm();
}

EWXWEXPORT(void,wxLayoutAlgorithm_Delete)(wxLayoutAlgorithm* self)
{
	delete self;
}

EWXWEXPORT(bool,wxLayoutAlgorithm_LayoutMDIFrame)(wxLayoutAlgorithm* self,wxMDIParentFrame* frame,int x,int y,int w,int h,int use)
{
	wxRect* r = NULL;
	if (use) r = new wxRect(x, y, w, h);
	
	bool result = self->LayoutMDIFrame(frame, r);
	
	if (r) delete r;
	return result;
}
	
EWXWEXPORT(bool,wxLayoutAlgorithm_LayoutFrame)(wxLayoutAlgorithm* self,wxFrame* frame,wxWindow* mainWindow)
{
	return self->LayoutFrame(frame, mainWindow);
}
	
EWXWEXPORT(bool,wxLayoutAlgorithm_LayoutWindow)(wxLayoutAlgorithm* self,wxFrame* frame,wxWindow* mainWindow)
{
	return self->LayoutWindow(frame, mainWindow);
}
	
}
