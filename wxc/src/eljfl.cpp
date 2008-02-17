#include "wrapper.h"
#include "wx/fl/toolwnd.h"
#include "wx/fl/antiflickpl.h"
#include "wx/fl/bardragpl.h"
#include "wx/fl/barhintspl.h"
#include "wx/fl/cbcustom.h"
#include "wx/fl/dyntbar.h"
#include "wx/fl/dyntbarhnd.h"
#include "wx/fl/gcupdatesmgr.h"
#include "wx/fl/hintanimpl.h"
#include "wx/fl/newbmpbtn.h"
#include "wx/fl/panedrawpl.h"
#include "wx/fl/rowdragpl.h"
#include "wx/fl/rowlayoutpl.h"

typedef void (*TButtonClick)(void* _obj, int _btn);
typedef int  (*TTitleClick)(void* _obj, void* _evt);
typedef void (*TMiniDraw)(void* _obj, void* _dct);

class ELJToolWindow : public wxToolWindow
{
    private:
        void*        EifObj;
        TButtonClick BtnCb;
        TTitleClick  TtlCb;
    public:
        ELJToolWindow(void* _obj, void* _btn, void* _ttl) : wxToolWindow()
        {
        	EifObj = _obj;
        	BtnCb = (TButtonClick)_btn;
        	TtlCb = (TTitleClick)_ttl;
        }
    
    virtual void OnMiniButtonClicked( int btnIdx ) {BtnCb (EifObj, btnIdx);}
    virtual bool HandleTitleClick( wxMouseEvent& event ) { return TtlCb (EifObj, (void*)&event) != 0; }
};

class ELJMiniButton : public cbMiniButton
{
    private:
        void*     EifObj;
        TMiniDraw DrwCb;
	public:
        ELJMiniButton(void* _obj, void* _drw) : cbMiniButton()
        {
        	EifObj = _obj;
        	DrwCb = (TMiniDraw)_drw;
		}
		
		virtual void Draw(wxDC& dc) {DrwCb(EifObj, (void*)&dc);}
};

extern "C"
{

EWXWEXPORT(void*, wxToolWindow_Create)(void* _obj, void* _btn, void* _ttl)
{
    return (void*) new ELJToolWindow(_obj, _btn, _ttl);
}
EWXWEXPORT(void, wxToolWindow_SetClient)(void* _obj, void* _wnd)
{
	((ELJToolWindow*)_obj)->SetClient((wxWindow*)_wnd);
}

EWXWEXPORT(void*, wxToolWindow_GetClient)(void* _obj)
{
	return (void*)((ELJToolWindow*)_obj)->GetClient();
}

EWXWEXPORT(void, wxToolWindow_SetTitleFont)(void* _obj, void* _fnt)
{
	((ELJToolWindow*)_obj)->SetTitleFont (*((wxFont*)_fnt));
}

EWXWEXPORT(void, wxToolWindow_AddMiniButton)(void* _obj, void* _btn)
{
	((ELJToolWindow*)_obj)->AddMiniButton((cbMiniButton*)_btn);
}

EWXWEXPORT(void*,cbMiniButton_Create)()
{
	return (void*) new cbMiniButton();
}
	
EWXWEXPORT(void,cbMiniButton_Delete)(void* _obj)
{
	delete (cbMiniButton*)_obj;
}
	
EWXWEXPORT(void,cbMiniButton_Pos)(void* _obj, void* x, void* y)
{
	wxPoint pos = ((cbMiniButton*)_obj)->mPos;
	*((int*)x) = pos.x;
	*((int*)y) = pos.y;
}
	
EWXWEXPORT(void,cbMiniButton_Dim)(void* _obj, void* w, void* h)
{
	wxSize size = ((cbMiniButton*)_obj)->mDim;
	*((int*)w) = size.x;
	*((int*)h) = size.y;
}
	
EWXWEXPORT(int,cbMiniButton_Visible)(void* _obj)
{
	return (int)((cbMiniButton*)_obj)->mVisible;
}
	
EWXWEXPORT(int,cbMiniButton_Enabled)(void* _obj)
{
	return (int)((cbMiniButton*)_obj)->mEnabled;
}
	
EWXWEXPORT(void*,cbMiniButton_Layout)(void* _obj)
{
	return (void*)((cbMiniButton*)_obj)->mpLayout;
}
	
EWXWEXPORT(void*,cbMiniButton_Pane)(void* _obj)
{
	return (void*)((cbMiniButton*)_obj)->mpPane;
}
	
EWXWEXPORT(void*,cbMiniButton_Plugin)(void* _obj)
{
	return (void*)((cbMiniButton*)_obj)->mpPlugin;
}
	
EWXWEXPORT(void*,cbMiniButton_Wnd)(void* _obj)
{
	return (void*)((cbMiniButton*)_obj)->mpWnd;
}
	
EWXWEXPORT(int,cbMiniButton_DragStarted)(void* _obj)
{
	return (int)((cbMiniButton*)_obj)->mDragStarted;
}
	
EWXWEXPORT(int,cbMiniButton_Pressed)(void* _obj)
{
	return (int)((cbMiniButton*)_obj)->mPressed;
}
	
EWXWEXPORT(void,cbMiniButton_SetPos)(void* _obj, int x, int y)
{
	((cbMiniButton*)_obj)->SetPos(wxPoint(x, y));
}
	
EWXWEXPORT(int,cbMiniButton_HitTest)(void* _obj,  int x, int y)
{
	return (int)((cbMiniButton*)_obj)->HitTest(wxPoint(x, y));
}
	
EWXWEXPORT(void,cbMiniButton_Refresh)(void* _obj)
{
	((cbMiniButton*)_obj)->Refresh();
}
	
EWXWEXPORT(int,cbMiniButton_WasClicked)(void* _obj)
{
	return (int)((cbMiniButton*)_obj)->WasClicked();
}
	
EWXWEXPORT(void,cbMiniButton_Reset)(void* _obj)
{
	((cbMiniButton*)_obj)->Reset();
}
	
EWXWEXPORT(void,cbMiniButton_Enable)(void* _obj,  int enable)
{
	((cbMiniButton*)_obj)->Enable(enable != 0);
}
	
EWXWEXPORT(int,cbMiniButton_IsPressed)(void* _obj)
{
	return (int)((cbMiniButton*)_obj)->IsPressed();
}
	
EWXWEXPORT(void*,cbCloseBox_Create)()
{
	return (void*) new cbCloseBox();
}
	
EWXWEXPORT(void*,cbCollapseBox_Create)()
{
	return (void*) new cbCollapseBox();
}
	
EWXWEXPORT(void*,cbDockBox_Create)()
{
	return (void*) new cbDockBox();
}
	
EWXWEXPORT(void*,cbFloatedBarWindow_Create)(void* _obj)
{
	return (void*) new cbFloatedBarWindow();
}
	
EWXWEXPORT(void,cbFloatedBarWindow_SetBar)(void* _obj, void* _bar)
{
	((cbFloatedBarWindow*)_obj)->SetBar((cbBarInfo*)_bar);
}
	
EWXWEXPORT(void,cbFloatedBarWindow_SetLayout)(void* _obj, void* _layout)
{
	((cbFloatedBarWindow*)_obj)->SetLayout((wxFrameLayout*)_layout);
}
	
EWXWEXPORT(void*,cbFloatedBarWindow_GetBar)(void* _obj)
{
	return (void*)((cbFloatedBarWindow*)_obj)->GetBar();
}
	
EWXWEXPORT(void,cbFloatedBarWindow_PositionFloatedWnd)(void* _obj, int _x, int _y, int _w, int _h)
{
	((cbFloatedBarWindow*)_obj)->PositionFloatedWnd(_x, _y, _w, _h);
}
	
EWXWEXPORT(void*,wxFrameLayout_Create)(void* pParentFrame, void* pFrameClient, int activateNow)
{
	return (void*) new wxFrameLayout((wxWindow*)pParentFrame, (wxWindow*)pFrameClient, activateNow != 0);
}
	
EWXWEXPORT(void,wxFrameLayout_Delete)(void* _obj)
{
	delete (wxFrameLayout*)_obj;
}

EWXWEXPORT(void,wxFrameLayout_EnableFloating)(void* _obj, int enable)
{
	((wxFrameLayout*)_obj)->EnableFloating(enable != 0);
}
	
EWXWEXPORT(void,wxFrameLayout_Activate)(void* _obj)
{
	((wxFrameLayout*)_obj)->Activate();
}
	
EWXWEXPORT(void,wxFrameLayout_Deactivate)(void* _obj)
{
	((wxFrameLayout*)_obj)->Deactivate();
}

EWXWEXPORT(void,wxFrameLayout_HideBarWindows)(void* _obj)
{
	((wxFrameLayout*)_obj)->HideBarWindows();
}
	
EWXWEXPORT(void,wxFrameLayout_DestroyBarWindows)(void* _obj)
{
	return ((wxFrameLayout*)_obj)->DestroyBarWindows();
}
	
EWXWEXPORT(void,wxFrameLayout_SetFrameClient)(void* _obj, void* pFrameClient)
{
	((wxFrameLayout*)_obj)->SetFrameClient((wxWindow*)pFrameClient);
}
	
EWXWEXPORT(void*,wxFrameLayout_GetFrameClient)(void* _obj)
{
	return (void*)((wxFrameLayout*)_obj)->GetFrameClient();
}
	
EWXWEXPORT(void*,wxFrameLayout_GetParentFrame)(void* _obj)
{
	return (void*)((wxFrameLayout*)_obj)->mpFrame;
}
	
EWXWEXPORT(void*,wxFrameLayout_GetPane)(void* _obj, int alignment)
{
	return (void*)((wxFrameLayout*)_obj)->GetPane(alignment);
}
	
EWXWEXPORT(void,wxFrameLayout_AddBar)(void* _obj, void* pBarWnd, void* dimInfo, int alignment, int rowNo, int columnPos, void* name, int spyEvents, int state)
{
	((wxFrameLayout*)_obj)->AddBar((wxWindow*)pBarWnd, *((cbDimInfo*)dimInfo), alignment, rowNo, columnPos, (char*)name, spyEvents != 0, state);
}
	
EWXWEXPORT(int,wxFrameLayout_RedockBar)(void* _obj, void* pBar, int x, int y, int w, int h, void* pToPane, int updateNow)
{
	return (int)((wxFrameLayout*)_obj)->RedockBar((cbBarInfo*)pBar, wxRect(x, y, w, h), (cbDockPane*)pToPane, updateNow != 0);
}
	
EWXWEXPORT(void*,wxFrameLayout_FindBarByName)(void* _obj, void* name)
{
	return (void*)((wxFrameLayout*)_obj)->FindBarByName((char*)name);
}
	
EWXWEXPORT(void*,wxFrameLayout_FindBarByWindow)(void* _obj, void* pWnd)
{
	return (void*)((wxFrameLayout*)_obj)->FindBarByWindow((wxWindow*)pWnd);
}
	
EWXWEXPORT(int,wxFrameLayout_GetBars)(void* _obj, void* _ref)
{
	BarArrayT arr = ((wxFrameLayout*)_obj)->GetBars();
	
	if (_ref)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((void**)_ref)[i] = arr.Item(i);
	}
	
	return arr.GetCount();
}
	
EWXWEXPORT(void,wxFrameLayout_SetBarState)(void* _obj, void* pBar, int newStatem, int updateNow)
{
	((wxFrameLayout*)_obj)->SetBarState((cbBarInfo*)pBar, newStatem, updateNow != 0);
}
	
EWXWEXPORT(void,wxFrameLayout_InverseVisibility)(void* _obj, void* pBar)
{
	((wxFrameLayout*)_obj)->InverseVisibility((cbBarInfo*)pBar);
}
	
EWXWEXPORT(void,wxFrameLayout_ApplyBarProperties)(void* _obj, void* pBar)
{
	((wxFrameLayout*)_obj)->ApplyBarProperties((cbBarInfo*)pBar);
}
	
EWXWEXPORT(void,wxFrameLayout_RemoveBar)(void* _obj, void* pBar)
{
	((wxFrameLayout*)_obj)->RemoveBar((cbBarInfo*)pBar);
}
	
EWXWEXPORT(void,wxFrameLayout_RecalcLayout)(void* _obj, int repositionBarsNow)
{
	((wxFrameLayout*)_obj)->RecalcLayout(repositionBarsNow != 0);
}
	
EWXWEXPORT(int,wxFrameLayout_GetClientHeight)(void* _obj)
{
	return ((wxFrameLayout*)_obj)->GetClientHeight();
}
	
EWXWEXPORT(int,wxFrameLayout_GetClientWidth)(void* _obj)
{
	return ((wxFrameLayout*)_obj)->GetClientWidth();
}
	
EWXWEXPORT(void,wxFrameLayout_GetClientRect)(void* _obj, void* x, void* y, void* w, void* h)
{
	wxRect rect = ((wxFrameLayout*)_obj)->GetClientRect();
	*((int*)x) = rect.x;
	*((int*)y) = rect.y;
	*((int*)w) = rect.width;
	*((int*)h) = rect.height;
}
	
EWXWEXPORT(void*,wxFrameLayout_GetUpdatesManager)(void* _obj)
{
	return (void*)((wxFrameLayout*)_obj)->mpUpdatesMgr;
}
	
EWXWEXPORT(void,wxFrameLayout_SetUpdatesManager)(void* _obj, void* pUMgr)
{
	((wxFrameLayout*)_obj)->SetUpdatesManager((cbUpdatesManagerBase*)pUMgr);
}
	
EWXWEXPORT(void,wxFrameLayout_GetPaneProperties)(void* _obj, void* props, int alignment)
{
	((wxFrameLayout*)_obj)->GetPaneProperties(*((cbCommonPaneProperties*)props), alignment);
}
	
EWXWEXPORT(void,wxFrameLayout_SetPaneProperties)(void* _obj, void* props, int paneMask)
{
	((wxFrameLayout*)_obj)->SetPaneProperties(*((cbCommonPaneProperties*)props), paneMask);
}
	
EWXWEXPORT(void,wxFrameLayout_SetMargins)(void* _obj, int top, int bottom, int left, int right, int paneMask)
{
	((wxFrameLayout*)_obj)->SetMargins(top, bottom, left, right, paneMask);
}
	
EWXWEXPORT(void,wxFrameLayout_SetPaneBackground)(void* _obj, void* colour)
{
	((wxFrameLayout*)_obj)->SetPaneBackground(*((wxColour*)colour));
}
	
EWXWEXPORT(void,wxFrameLayout_RefreshNow)(void* _obj, int recalcLayout)
{
	((wxFrameLayout*)_obj)->RefreshNow(recalcLayout != 0);
}
	
EWXWEXPORT(void,wxFrameLayout_OnSize)(void* _obj, void* event)
{
	((wxFrameLayout*)_obj)->OnSize(*((wxSizeEvent*)event));
}
	
EWXWEXPORT(void,wxFrameLayout_OnLButtonDown)(void* _obj, void* event)
{
	((wxFrameLayout*)_obj)->OnLButtonDown(*((wxMouseEvent*)event));
}
	
EWXWEXPORT(void,wxFrameLayout_OnLDblClick)(void* _obj, void* event)
{
	((wxFrameLayout*)_obj)->OnLDblClick(*((wxMouseEvent*)event));
}
	
EWXWEXPORT(void,wxFrameLayout_OnLButtonUp)(void* _obj, void* event)
{
	((wxFrameLayout*)_obj)->OnLButtonUp(*((wxMouseEvent*)event));
}
	
EWXWEXPORT(void,wxFrameLayout_OnRButtonDown)(void* _obj, void* event)
{
	((wxFrameLayout*)_obj)->OnRButtonDown(*((wxMouseEvent*)event));
}
	
EWXWEXPORT(void,wxFrameLayout_OnRButtonUp)(void* _obj, void* event)
{
	((wxFrameLayout*)_obj)->OnRButtonUp(*((wxMouseEvent*)event));
}
	
EWXWEXPORT(void,wxFrameLayout_OnMouseMove)(void* _obj, void* event)
{
	((wxFrameLayout*)_obj)->OnMouseMove(*((wxMouseEvent*)event));
}
	
EWXWEXPORT(void,wxFrameLayout_FirePluginEvent)(void* _obj, void* event)
{
	((wxFrameLayout*)_obj)->FirePluginEvent(*((cbPluginEvent*)event));
}
	
EWXWEXPORT(void,wxFrameLayout_CaptureEventsForPlugin)(void* _obj, void* pPlugin)
{
	((wxFrameLayout*)_obj)->CaptureEventsForPlugin((cbPluginBase*)pPlugin);
}
	
EWXWEXPORT(void,wxFrameLayout_ReleaseEventsFromPlugin)(void* _obj, void* pPlugin)
{
	((wxFrameLayout*)_obj)->ReleaseEventsFromPlugin((cbPluginBase*)pPlugin);
}
	
EWXWEXPORT(void,wxFrameLayout_CaptureEventsForPane)(void* _obj, void* toPane)
{
	((wxFrameLayout*)_obj)->CaptureEventsForPane((cbDockPane*)toPane);
}
	
EWXWEXPORT(void,wxFrameLayout_ReleaseEventsFromPane)(void* _obj, void* fromPane)
{
	((wxFrameLayout*)_obj)->ReleaseEventsFromPane((cbDockPane*)fromPane);
}
	
EWXWEXPORT(void*,wxFrameLayout_GetTopPlugin)(void* _obj)
{
	return (void*)((wxFrameLayout*)_obj)->mpTopPlugin;
}
	
EWXWEXPORT(void,wxFrameLayout_SetTopPlugin)(void* _obj, void* pPlugin)
{
	((wxFrameLayout*)_obj)->SetTopPlugin((cbPluginBase*)pPlugin);
}
	
EWXWEXPORT(void,wxFrameLayout_PushPlugin)(void* _obj, void* pPugin)
{
	((wxFrameLayout*)_obj)->PushPlugin((cbPluginBase*)pPugin);
}
	
EWXWEXPORT(void,wxFrameLayout_PopPlugin)(void* _obj)
{
	((wxFrameLayout*)_obj)->PopPlugin();
}
	
EWXWEXPORT(void,wxFrameLayout_PopAllPlugins)(void* _obj)
{
	((wxFrameLayout*)_obj)->PopAllPlugins();
}
	
EWXWEXPORT(void,wxFrameLayout_PushDefaultPlugins)(void* _obj)
{
	((wxFrameLayout*)_obj)->PushDefaultPlugins();
}
	
EWXWEXPORT(void,wxFrameLayout_AddPlugin)(void* _obj, void* pPlInfo, int paneMask)
{
	((wxFrameLayout*)_obj)->AddPlugin((wxClassInfo*)pPlInfo, paneMask);
}
	
EWXWEXPORT(void,wxFrameLayout_AddPluginBefore)(void* _obj, void* pNextPlInfo, void* pPlInfo, int paneMask)
{
	((wxFrameLayout*)_obj)->AddPluginBefore((wxClassInfo*)pNextPlInfo, (wxClassInfo*)pPlInfo, paneMask);
}
	
EWXWEXPORT(void,wxFrameLayout_RemovePlugin)(void* _obj, void* pPlInfo)
{
	((wxFrameLayout*)_obj)->RemovePlugin((wxClassInfo*)pPlInfo);
}
	
EWXWEXPORT(void*,wxFrameLayout_FindPlugin)(void* _obj, void* pPlInfo )
{
	return (void*)((wxFrameLayout*)_obj)->FindPlugin((wxClassInfo*)pPlInfo);
}
	
EWXWEXPORT(int,wxFrameLayout_HasTopPlugin)(void* _obj)
{
	return (int)((wxFrameLayout*)_obj)->HasTopPlugin();
}
	
EWXWEXPORT(void*,cbDimInfo_CreateDefault)()
{
	return (void*) new cbDimInfo();
}
	
EWXWEXPORT(void*,cbDimInfo_CreateWithHandler)(void* pDimHandler, int isFixed)
{
	return (void*) new cbDimInfo((cbBarDimHandlerBase*)pDimHandler, isFixed != 0);
}
	
EWXWEXPORT(void*,cbDimInfo_CreateWithInfo)(int dh_x, int dh_y, int dv_x, int dv_y, int f_x, int f_y, int isFixed, int horizGap, int vertGap, void* pDimHandler)
{
	return (void*) new cbDimInfo(dh_x, dh_y, dv_x, dv_y, f_x, f_y, isFixed != 0, horizGap, vertGap, (cbBarDimHandlerBase*)pDimHandler);
}
	
EWXWEXPORT(void*,cbDimInfo_Create)(int x, int y, bool isFixed, int gap, void* pDimHandler)
{
	return (void*) new cbDimInfo(x, y, isFixed != 0, gap, (cbBarDimHandlerBase*)pDimHandler);
}
	
EWXWEXPORT(void,cbDimInfo_Delete)(void* _obj)
{
	delete (cbDimInfo*)_obj;
}

EWXWEXPORT(void,cbDimInfo_Assign)(void* _obj, void* other)
{
	*((cbDimInfo*)_obj) = *((cbDimInfo*)other);
}
	
EWXWEXPORT(void*,cbDimInfo_GetDimHandler)(void* _obj)
{
	return (void*)((cbDimInfo*)_obj)->GetDimHandler();
}
	
EWXWEXPORT(void*,cbBarInfo_Create)()
{
	return (void*) new cbBarInfo();
}
	
EWXWEXPORT(void,cbBarInfo_Delete)(void* _obj)
{
	delete (cbBarInfo*)_obj;
}
	
EWXWEXPORT(int,cbBarInfo_IsFixed)(void* _obj)
{
	return (int)((cbBarInfo*)_obj)->IsFixed();
}
	
EWXWEXPORT(int,cbBarInfo_IsExpanded)(void* _obj)
{
	return (int)((cbBarInfo*)_obj)->IsExpanded();
}
	
EWXWEXPORT(void*,cbBarSpy_CreateDefault)()
{
	return (void*) new cbBarSpy();
}
	
EWXWEXPORT(void*,cbBarSpy_Create)(void* pPanel)
{
	return (void*) new cbBarSpy((wxFrameLayout*)pPanel);
}
	
EWXWEXPORT(void,cbBarSpy_Delete)(void* _obj)
{
	delete (cbBarSpy*)_obj;
}
	
EWXWEXPORT(void,cbBarSpy_SetBarWindow)(void* _obj, void* pWnd)
{
	((cbBarSpy*)_obj)->SetBarWindow((wxWindow*)pWnd);
}
	
EWXWEXPORT(int,cbBarSpy_ProcessEvent)(void* _obj, void* event)
{
	return (int)((cbBarSpy*)_obj)->ProcessEvent(*((wxEvent*)event));
}
	
EWXWEXPORT(void*,cbRowInfo_Create)()
{
	return (void*) new cbRowInfo();
}
	
EWXWEXPORT(void,cbRowInfo_Delete)(void* _obj)
{
	delete (cbRowInfo*)_obj;
}
	
EWXWEXPORT(void*,cbRowInfo_GetFirstBar)(void* _obj)
{
	return (void*)((cbRowInfo*)_obj)->GetFirstBar();
}
	
EWXWEXPORT(void*,cbDockPane_CreateDefault)()
{
	return (void*) new cbDockPane();
}
	
EWXWEXPORT(void*,cbDockPane_Create)(int alignment, void* pPanel)
{
	return (void*) new cbDockPane(alignment, (wxFrameLayout*)pPanel);
}
	
EWXWEXPORT(void,cbDockPane_Delete)(void* _obj)
{
	delete (cbDockPane*)_obj;
}
	
EWXWEXPORT(void,cbDockPane_SetMargins)(void* _obj, int top, int bottom, int left, int right)
{
	((cbDockPane*)_obj)->SetMargins(top, bottom, left, right);
}
	
EWXWEXPORT(void,cbDockPane_RemoveBar)(void* _obj, void* pBar)
{
	((cbDockPane*)_obj)->RemoveBar((cbBarInfo*)pBar);
}
	
EWXWEXPORT(void,cbDockPane_InsertBarByCoord)(void* _obj, void* pBar, int x, int y, int w, int h)
{
	((cbDockPane*)_obj)->InsertBar((cbBarInfo*)pBar, wxRect(x, y, w, h));
}
	
EWXWEXPORT(void,cbDockPane_InsertBarToRow)(void* _obj, void* pBar, void* pIntoRow)
{
	((cbDockPane*)_obj)->InsertBar((cbBarInfo*)pBar, (cbRowInfo*)pIntoRow);
}
	
EWXWEXPORT(void,cbDockPane_InsertBarByInfo)(void* _obj, void* pBarInfo)
{
	((cbDockPane*)_obj)->InsertBar((cbBarInfo*)pBarInfo);
}
	
EWXWEXPORT(void,cbDockPane_RemoveRow)(void* _obj, void* pRow)
{
	((cbDockPane*)_obj)->RemoveRow((cbRowInfo*)pRow);
}
	
EWXWEXPORT(void,cbDockPane_InsertRow)(void* _obj, void* pRow, void* pBeforeRow)
{
	((cbDockPane*)_obj)->InsertRow((cbRowInfo*)pRow, (cbRowInfo*)pBeforeRow);
}
	
EWXWEXPORT(void,cbDockPane_SetPaneWidth)(void* _obj, int width)
{
	((cbDockPane*)_obj)->SetPaneWidth(width);
}
	
EWXWEXPORT(void,cbDockPane_SetBoundsInParent)(void* _obj, int _x, int _y, int _w, int _h)
{
	((cbDockPane*)_obj)->SetBoundsInParent(wxRect(_x, _y, _w, _h));
}
	
EWXWEXPORT(void,cbDockPane_GetRealRect)(void* _obj, void* _x, void* _y, void* _w, void* _h)
{
	wxRect rect = ((cbDockPane*)_obj)->GetRealRect();
	*((int*)_x) = rect.x;
	*((int*)_y) = rect.y;
	*((int*)_w) = rect.width;
	*((int*)_h) = rect.height;
}
	
EWXWEXPORT(int,cbDockPane_GetRowList)(void* _obj, void* _ref)
{
	RowArrayT arr = ((cbDockPane*)_obj)->GetRowList();
	
	if (_ref)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((void**)_ref)[i] = (void*)arr.Item(i);
	}
	
	return arr.GetCount();
}
	
EWXWEXPORT(void*,cbDockPane_GetFirstRow)(void* _obj)
{
	return (void*)((cbDockPane*)_obj)->GetFirstRow();
}
	
EWXWEXPORT(int,cbDockPane_BarPresent)(void* _obj, void* pBar)
{
	return (int)((cbDockPane*)_obj)->BarPresent((cbBarInfo*)pBar);
}
	
EWXWEXPORT(int,cbDockPane_GetPaneHeight)(void* _obj)
{
	return ((cbDockPane*)_obj)->GetPaneHeight();
}
	
EWXWEXPORT(int,cbDockPane_GetAlignment)(void* _obj)
{
	return ((cbDockPane*)_obj)->GetAlignment();
}
	
EWXWEXPORT(int,cbDockPane_MatchesMask)(void* _obj, int paneMask)
{
	return (int)((cbDockPane*)_obj)->MatchesMask(paneMask);
}
	
EWXWEXPORT(int,cbDockPane_IsHorizontal)(void* _obj)
{
	return (int)((cbDockPane*)_obj)->IsHorizontal();
}
	
EWXWEXPORT(int,cbDockPane_GetDockingState)(void* _obj)
{
	return ((cbDockPane*)_obj)->GetDockingState();
}
	
EWXWEXPORT(int,cbDockPane_HitTestPaneItems)(void* _obj, int x, int y, void* ppRow, void* ppBar)
{
	return ((cbDockPane*)_obj)->HitTestPaneItems(wxPoint(x, y), (cbRowInfo**)ppRow, (cbBarInfo**)ppBar);
}
	
EWXWEXPORT(void,cbDockPane_GetBarResizeRange)(void* _obj, void* pBar, void* from, void* till, int forLeftHandle)
{
	((cbDockPane*)_obj)->GetBarResizeRange((cbBarInfo*)pBar, (int*)from, (int*)till, forLeftHandle != 0);
}
	
EWXWEXPORT(void,cbDockPane_GetRowResizeRange)(void* _obj, void* pRow, void* from, void* till, int forUpperHandle)
{
	((cbDockPane*)_obj)->GetRowResizeRange((cbRowInfo*)pRow, (int*)from, (int*)till, forUpperHandle != 0);
}
	
EWXWEXPORT(void*,cbDockPane_GetBarInfoByWindow)(void* _obj, void* pBarWnd)
{
	return (void*)((cbDockPane*)_obj)->GetBarInfoByWindow((wxWindow*)pBarWnd);
}
	
EWXWEXPORT(int,cbPluginBase_GetPaneMask)(void* _obj)
{
	return ((cbPluginBase*)_obj)->GetPaneMask();
}

EWXWEXPORT(void,cbPluginBase_Delete)(void* _obj)
{
	delete (cbPluginBase*)_obj;
}

EWXWEXPORT(int,cbPluginBase_IsReady)(void* _obj)
{
	return (int)((cbPluginBase*)_obj)->IsReady();
}
	
EWXWEXPORT(int,cbPluginBase_ProcessEvent)(void* _obj, void* event)
{
	return (int)((cbPluginBase*)_obj)->ProcessEvent(*((wxEvent*)event));
}
	
EWXWEXPORT(void*,cbPluginEvent_Pane)(void* _obj)
{
	return ((cbPluginEvent*)_obj)->mpPane;
}

EWXWEXPORT(void,cbLeftDownEvent_Pos)(void* _obj, void* _x, void* _y)
{
	wxPoint pos = ((cbLeftDownEvent*)_obj)->mPos;
	*((int*)_x) = pos.x;
	*((int*)_y) = pos.y;
}
	
EWXWEXPORT(void,cbLeftUpEvent_Pos)(void* _obj, void* _x, void* _y)
{
	wxPoint pos = ((cbLeftUpEvent*)_obj)->mPos;
	*((int*)_x) = pos.x;
	*((int*)_y) = pos.y;
}
	
EWXWEXPORT(void,cbRightDownEvent_Pos)(void* _obj, void* _x, void* _y)
{
	wxPoint pos = ((cbRightDownEvent*)_obj)->mPos;
	*((int*)_x) = pos.x;
	*((int*)_y) = pos.y;
}
	
EWXWEXPORT(void,cbRightUpEvent_Pos)(void* _obj, void* _x, void* _y)
{
	wxPoint pos = ((cbRightUpEvent*)_obj)->mPos;
	*((int*)_x) = pos.x;
	*((int*)_y) = pos.y;
}
	
EWXWEXPORT(void,cbMotionEvent_Pos)(void* _obj, void* _x, void* _y)
{
	wxPoint pos = ((cbMotionEvent*)_obj)->mPos;
	*((int*)_x) = pos.x;
	*((int*)_y) = pos.y;
}
	
EWXWEXPORT(void,cbLeftDClickEvent_Pos)(void* _obj, void* _x, void* _y)
{
	wxPoint pos = ((cbLeftDClickEvent*)_obj)->mPos;
	*((int*)_x) = pos.x;
	*((int*)_y) = pos.y;
}
	
EWXWEXPORT(void*,cbLayoutRowEvent_Row)(void* _obj)
{
	return (void*)((cbLayoutRowEvent*)_obj)->mpRow;
}

EWXWEXPORT(void*,cbResizeRowEvent_Row)(void* _obj)
{
	return (void*)((cbResizeRowEvent*)_obj)->mpRow;
}

EWXWEXPORT(int,cbResizeRowEvent_HandleOfs)(void* _obj)
{
	return ((cbResizeRowEvent*)_obj)->mHandleOfs;
}
	
EWXWEXPORT(int,cbResizeRowEvent_ForUpperHandle)(void* _obj)
{
	return (int)((cbResizeRowEvent*)_obj)->mForUpperHandle;
}
	
EWXWEXPORT(void*,cbInsertBarEvent_Bar)(void* _obj)
{
	return (void*)((cbInsertBarEvent*)_obj)->mpBar;
}
	
EWXWEXPORT(void*,cbInsertBarEvent_Row)(void* _obj)
{
	return (void*)((cbInsertBarEvent*)_obj)->mpRow;
}
	
EWXWEXPORT(void*,cbResizeBarEvent_Bar)(void* _obj)
{
	return (void*)((cbResizeBarEvent*)_obj)->mpBar;
}
	
EWXWEXPORT(void*,cbResizeBarEvent_Row)(void* _obj)
{
	return (void*)((cbResizeBarEvent*)_obj)->mpRow;
}
	
EWXWEXPORT(void*,cbRemoveBarEvent_Bar)(void* _obj)
{
	return (void*)((cbRemoveBarEvent*)_obj)->mpBar;
}
	
EWXWEXPORT(void*,cbSizeBarWndEvent_Bar)(void* _obj)
{
	return (void*)((cbSizeBarWndEvent*)_obj)->mpBar;
}
	
EWXWEXPORT(void,cbSizeBarWndEvent_BoundsInParent)(void* _obj, void* _x, void* _y, void* _w, void* _h)
{
	wxRect rect = ((cbSizeBarWndEvent*)_obj)->mBoundsInParent;
	*((int*)_x) = rect.x;
	*((int*)_y) = rect.y;
	*((int*)_w) = rect.width;
	*((int*)_h) = rect.height;
}
	
EWXWEXPORT(void*,cbDrawBarDecorEvent_Bar)(void* _obj)
{
	return (void*)((cbDrawBarDecorEvent*)_obj)->mpBar;
}
	
EWXWEXPORT(void,cbDrawBarDecorEvent_BoundsInParent)(void* _obj, void* _x, void* _y, void* _w, void* _h)
{
	wxRect rect = ((cbDrawBarDecorEvent*)_obj)->mBoundsInParent;
	*((int*)_x) = rect.x;
	*((int*)_y) = rect.y;
	*((int*)_w) = rect.width;
	*((int*)_h) = rect.height;
}
	
EWXWEXPORT(void*,cbDrawBarDecorEvent_Dc)(void* _obj)
{
	return (void*)((cbDrawBarDecorEvent*)_obj)->mpDc;
}
	
EWXWEXPORT(void*,cbDrawRowDecorEvent_Row)(void* _obj)
{
	return (void*)((cbDrawRowDecorEvent*)_obj)->mpRow;
}
	
EWXWEXPORT(void*,cbDrawRowDecorEvent_Dc)(void* _obj)
{
	return (void*)((cbDrawRowDecorEvent*)_obj)->mpDc;
}
	
EWXWEXPORT(void*,cbDrawPaneDecorEvent_Dc)(void* _obj)
{
	return (void*)((cbDrawPaneDecorEvent*)_obj)->mpDc;
}
	
EWXWEXPORT(void*,cbDrawBarHandlesEvent_Bar)(void* _obj)
{
	return (void*)((cbDrawBarHandlesEvent*)_obj)->mpBar;
}
	
EWXWEXPORT(void*,cbDrawBarHandlesEvent_Dc)(void* _obj)
{
	return (void*)((cbDrawBarHandlesEvent*)_obj)->mpDc;
}
	
EWXWEXPORT(void*,cbDrawRowHandlesEvent_Row)(void* _obj)
{
	return (void*)((cbDrawRowHandlesEvent*)_obj)->mpRow;
}
	
EWXWEXPORT(void*,cbDrawRowHandlesEvent_Dc)(void* _obj)
{
	return (void*)((cbDrawRowHandlesEvent*)_obj)->mpDc;
}
	
EWXWEXPORT(void*,cbDrawRowBkGroundEvent_Row)(void* _obj)
{
	return (void*)((cbDrawRowBkGroundEvent*)_obj)->mpRow;
}
	
EWXWEXPORT(void*,cbDrawRowBkGroundEvent_Dc)(void* _obj)
{
	return (void*)((cbDrawRowBkGroundEvent*)_obj)->mpDc;
}
	
EWXWEXPORT(void*,cbDrawPaneBkGroundEvent_Dc)(void* _obj)
{
	return (void*)((cbDrawPaneBkGroundEvent*)_obj)->mpDc;
}
	
EWXWEXPORT(void*,cbStartBarDraggingEvent_Bar)(void* _obj)
{
	return (void*)((cbStartBarDraggingEvent*)_obj)->mpBar;
}
	
EWXWEXPORT(void,cbStartBarDraggingEvent_Pos)(void* _obj, void* _x, void* _y)
{
	wxPoint pos = ((cbStartBarDraggingEvent*)_obj)->mPos;
	*((int*)_x) = pos.x;
	*((int*)_y) = pos.y;
}
	
EWXWEXPORT(void,cbDrawHintRectEvent_Rect)(void* _obj, void* _x, void* _y, void* _w, void* _h)
{
	wxRect rect = ((cbDrawHintRectEvent*)_obj)->mRect;
	*((int*)_x) = rect.x;
	*((int*)_y) = rect.y;
	*((int*)_w) = rect.width;
	*((int*)_h) = rect.height;
}
	
EWXWEXPORT(int,cbDrawHintRectEvent_LastTime)(void* _obj)
{
	return (int)((cbDrawHintRectEvent*)_obj)->mLastTime;
}
	
EWXWEXPORT(int,cbDrawHintRectEvent_EraseRect)(void* _obj)
{
	return (int)((cbDrawHintRectEvent*)_obj)->mEraseRect;
}
	
EWXWEXPORT(int,cbDrawHintRectEvent_IsInClient)(void* _obj)
{
	return (int)((cbDrawHintRectEvent*)_obj)->mIsInClient;
}
	
EWXWEXPORT(void,cbStartDrawInAreaEvent_Area)(void* _obj, void* _x, void* _y, void* _w, void* _h)
{
	wxRect rect = ((cbStartDrawInAreaEvent*)_obj)->mArea;
	*((int*)_x) = rect.x;
	*((int*)_y) = rect.y;
	*((int*)_w) = rect.width;
	*((int*)_h) = rect.height;
}
	
EWXWEXPORT(void,cbFinishDrawInAreaEvent_Area)(void* _obj, void* _x, void* _y, void* _w, void* _h)
{
	wxRect rect = ((cbFinishDrawInAreaEvent*)_obj)->mArea;
	*((int*)_x) = rect.x;
	*((int*)_y) = rect.y;
	*((int*)_w) = rect.width;
	*((int*)_h) = rect.height;
}
	
EWXWEXPORT(void,cbCustomizeBarEvent_ClickPos)(void* _obj, void* _x, void* _y)
{
	wxPoint pos = ((cbCustomizeBarEvent*)_obj)->mClickPos;
	*((int*)_x) = pos.x;
	*((int*)_y) = pos.y;
}
	
EWXWEXPORT(void*,cbCustomizeBarEvent_Bar)(void* _obj)
{
	return (void*)((cbCustomizeBarEvent*)_obj)->mpBar;
}
	
EWXWEXPORT(void,cbCustomizeLayoutEvent_ClickPos)(void* _obj, void* _x, void* _y)
{
	wxPoint pos = ((cbCustomizeLayoutEvent*)_obj)->mClickPos;
	*((int*)_x) = pos.x;
	*((int*)_y) = pos.y;
}
	
EWXWEXPORT(void*,cbAntiflickerPlugin_CreateDefault)()
{
	return (void*) new cbAntiflickerPlugin();
}
	
EWXWEXPORT(void*,cbAntiflickerPlugin_Create)(void* pPanel, int paneMask)
{
	return (void*) new cbAntiflickerPlugin((wxFrameLayout*)pPanel, paneMask);
}
	
EWXWEXPORT(void,cbAntiflickerPlugin_Delete)(void* _obj)
{
	delete (cbAntiflickerPlugin*)_obj;
}

EWXWEXPORT(void*,cbBarDragPlugin_CreateDefault)()
{
	return (void*) new cbBarDragPlugin();
}
	
EWXWEXPORT(void*,cbBarDragPlugin_Create)(void* pPanel, int paneMask)
{
	return (void*) new cbBarDragPlugin((wxFrameLayout*)pPanel, paneMask);
}
	
EWXWEXPORT(void,cbBarDragPlugin_Delete)(void* _obj)
{
	delete (cbBarDragPlugin*)_obj;
}

EWXWEXPORT(void*,cbBarHintsPlugin_CreateDefault)()
{
	return (void*) new cbBarHintsPlugin();
}
	
EWXWEXPORT(void*,cbBarHintsPlugin_Create)(void* pPanel, int paneMask)
{
	return (void*) new cbBarHintsPlugin((wxFrameLayout*)pPanel, paneMask);
}
	
EWXWEXPORT(void,cbBarHintsPlugin_Delete)(void* _obj)
{
	delete (cbBarHintsPlugin*)_obj;
}

EWXWEXPORT(void,cbBarHintsPlugin_SetGrooveCount)(void* _obj, int nGrooves)
{
	((cbBarHintsPlugin*)_obj)->SetGrooveCount(nGrooves);
}
	
EWXWEXPORT(void*,cbSimpleCustomizationPlugin_CreateDefault)()
{
	return (void*) new cbSimpleCustomizationPlugin();
}
	
EWXWEXPORT(void*,cbSimpleCustomizationPlugin_Create)(void* pPanel, int paneMask)
{
	return (void*) new cbSimpleCustomizationPlugin((wxFrameLayout*)pPanel, paneMask);
}
	
EWXWEXPORT(void,cbSimpleCustomizationPlugin_Delete)(void* _obj)
{
	delete (cbSimpleCustomizationPlugin*)_obj;
}

EWXWEXPORT(int,wxToolLayoutItem_IsSeparator)(void* _obj)
{
	return (int)((wxToolLayoutItem*)_obj)->mIsSeparator;
}
	
EWXWEXPORT(void,wxToolLayoutItem_Rect)(void* _obj, void* _x, void* _y, void* _w, void* _h)
{
	wxRect rect = ((wxToolLayoutItem*)_obj)->mRect;
	*((int*)_x) = rect.x;
	*((int*)_y) = rect.y;
	*((int*)_w) = rect.width;
	*((int*)_h) = rect.height;
}
	
EWXWEXPORT(void*,wxDynToolInfo_pToolWnd)(void* _obj)
{
	return (void*)((wxDynToolInfo*)_obj)->mpToolWnd;
}
	
EWXWEXPORT(int,wxDynToolInfo_Index)(void* _obj)
{
	return ((wxDynToolInfo*)_obj)->mIndex;
}
	
EWXWEXPORT(void,wxDynToolInfo_RealSize)(void* _obj, void* _w, void* _h)
{
	wxSize size = ((wxDynToolInfo*)_obj)->mRealSize;
	*((int*)_w) = size.x;
	*((int*)_h) = size.y;
}
	
EWXWEXPORT(void*,wxDynamicToolBar_CreateDefault)()
{
	return (void*) new wxDynamicToolBar();
}
	
EWXWEXPORT(void*,wxDynamicToolBar_Create)(void* parent, int id, int x, int y, int w, int h, int style, int orientation, int RowsOrColumns)
{
	return (void*) new wxDynamicToolBar((wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), (long)style, orientation, RowsOrColumns);
}

EWXWEXPORT(void,wxDynamicToolBar_Delete)(void* _obj)
{
	delete (wxDynamicToolBar*)_obj;
}

EWXWEXPORT(int,wxDynamicToolBar_CreateParams)(void* _obj, void* parent, int id, int x, int y, int w, int h, int style, int orientation, int RowsOrColumns)
{
	return (int)((wxDynamicToolBar*)_obj)->Create((wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), (long)style, orientation, RowsOrColumns);
}
	
EWXWEXPORT(void,wxDynamicToolBar_AddTool)(void* _obj, int toolIndex, void* pToolWindow, int w, int h)
{
	((wxDynamicToolBar*)_obj)->AddTool(toolIndex, (wxWindow*)pToolWindow, wxSize(w, h));
}
	
EWXWEXPORT(void,wxDynamicToolBar_AddToolImage)(void* _obj, int toolIndex, void* imageFileName, int imageFileType, void* labelText, int alignTextRight, int isFlat)
{
	((wxDynamicToolBar*)_obj)->AddTool(toolIndex, (char*)imageFileName, (wxBitmapType)imageFileType, (char*)labelText, alignTextRight != 0, isFlat != 0);
}
	
EWXWEXPORT(void,wxDynamicToolBar_AddToolLabel)(void* _obj, int toolIndex, void* labelBmp, void* labelText, int alignTextRight, int isFlat)
{
	((wxDynamicToolBar*)_obj)->AddTool(toolIndex, *((wxBitmap*)labelBmp), (char*)labelText, alignTextRight != 0, isFlat != 0);
}
	
EWXWEXPORT(void*,wxDynamicToolBar_AddToolBitmap)(void* _obj, int toolIndex, void* bitmap, void* pushedBitmap, int toggle, int x, int y, void* clientData, void* helpString1, void* helpString2)
{
	return (void*)((wxDynamicToolBar*)_obj)->AddTool(toolIndex, *((wxBitmap*)bitmap), *((wxBitmap*)pushedBitmap), toggle != 0, (long)x, (long)y, (wxObject*)clientData, (char*)helpString1, (char*)helpString2);
}
	
EWXWEXPORT(void,wxDynamicToolBar_AddSeparator)(void* _obj, void* pSepartorWnd)
{
	((wxDynamicToolBar*)_obj)->AddSeparator((wxWindow*)pSepartorWnd);
}
	
EWXWEXPORT(void*,wxDynamicToolBar_GetToolInfo)(void* _obj, int toolIndex)
{
	return (void*)((wxDynamicToolBar*)_obj)->GetToolInfo(toolIndex);
}
	
EWXWEXPORT(void,wxDynamicToolBar_RemoveTool)(void* _obj, int toolIndex)
{
	((wxDynamicToolBar*)_obj)->RemveTool(toolIndex);
}
	
EWXWEXPORT(void,wxDynamicToolBar_DrawSeparator)(void* _obj, void* info, void* dc)
{
	((wxDynamicToolBar*)_obj)->DrawSeparator(*((wxDynToolInfo*)info), *((wxDC*)dc));
}
	
EWXWEXPORT(int,wxDynamicToolBar_Layout)(void* _obj)
{
	return (int)((wxDynamicToolBar*)_obj)->Layout();
}
	
EWXWEXPORT(void,wxDynamicToolBar_GetPreferredDim)(void* _obj, int gw, int gh, void* pw, void* ph)
{
	wxSize size;
	((wxDynamicToolBar*)_obj)->GetPreferredDim(wxSize(gw, gh), size);
	*((int*)pw) = size.x;
	*((int*)ph) = size.y;
}
	
EWXWEXPORT(void*,wxDynamicToolBar_CreateDefaultLayout)(void* _obj)
{
	return (void*)((wxDynamicToolBar*)_obj)->CreateDefaultLayout();
}
	
EWXWEXPORT(void,wxDynamicToolBar_SetLayout)(void* _obj, void* pLayout)
{
	((wxDynamicToolBar*)_obj)->SetLayout((LayoutManagerBase*)pLayout);
}
	
EWXWEXPORT(void,wxDynamicToolBar_EnableTool)(void* _obj, int toolIndex, int enable)
{
	((wxDynamicToolBar*)_obj)->EnableTool(toolIndex, enable != 0);
}
	
EWXWEXPORT(void*,wxDynamicToolBar_FindToolForPosition)(void* _obj, int x, int y)
{
	return (void*)((wxDynamicToolBar*)_obj)->FindToolForPosition((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(int,wxDynamicToolBar_DoInsertTool)(void* _obj, int pos, void* tool)
{
	return (int)((wxDynamicToolBar*)_obj)->DoInsertTool((size_t)pos, (wxToolBarToolBase*)tool);
}
	
EWXWEXPORT(int,wxDynamicToolBar_DoDeleteTool)(void* _obj, int pos, void* tool)
{
	return (int)((wxDynamicToolBar*)_obj)->DoDeleteTool((size_t)pos, (wxToolBarToolBase*)tool);
}
	
EWXWEXPORT(void,wxDynamicToolBar_DoEnableTool)(void* _obj, void* tool, int enable)
{
	((wxDynamicToolBar*)_obj)->DoEnableTool((wxToolBarToolBase*)tool, enable != 0);
}
	
EWXWEXPORT(void,wxDynamicToolBar_DoToggleTool)(void* _obj, void* tool, int toggle)
{
	((wxDynamicToolBar*)_obj)->DoToggleTool((wxToolBarToolBase*)tool, toggle != 0);
}
	
EWXWEXPORT(void,wxDynamicToolBar_DoSetToggle)(void* _obj, void* tool, int toggle)
{
	((wxDynamicToolBar*)_obj)->DoSetToggle((wxToolBarToolBase*)tool, toggle != 0);
}
	
EWXWEXPORT(void*,wxDynamicToolBar_CreateTool)(void* _obj, int id, void* label, void* bmpNormal, void* bmpDisabled, int kind, void* clientData, void* shortHelp, void* longHelp)
{
	return (void*)((wxDynamicToolBar*)_obj)->CreateTool(id, (char*)label, *((wxBitmap*)bmpNormal), *((wxBitmap*)bmpDisabled), (wxItemKind)kind, (wxObject*)clientData, (char*)shortHelp, (char*)longHelp);
}
	
EWXWEXPORT(void*,wxDynamicToolBar_CreateToolControl)(void* _obj, void* control)
{
	return (void*)((wxDynamicToolBar*)_obj)->CreateTool((wxControl*)control);
}
	
EWXWEXPORT(void*,cbDynToolBarDimHandler_Create)()
{
	return (void*) new cbDynToolBarDimHandler();
}
	
EWXWEXPORT(void,cbDynToolBarDimHandler_Delete)(void* _obj)
{
	delete (cbDynToolBarDimHandler*)_obj;
}

EWXWEXPORT(void*,cbGCUpdatesMgr_Create)(void* pPanel)
{
	return (void*) new cbGCUpdatesMgr((wxFrameLayout*)pPanel);
}
	
EWXWEXPORT(void*,cbGCUpdatesMgr_CreateDefault)()
{
	return (void*) new cbGCUpdatesMgr();
}
	
EWXWEXPORT(void,cbGCUpdatesMgr_Delete)(void* _obj)
{
	delete (cbGCUpdatesMgr*)_obj;
}
	
EWXWEXPORT(void,cbGCUpdatesMgr_UpdateNow)(void* _obj)
{
	((cbGCUpdatesMgr*)_obj)->UpdateNow();
}

EWXWEXPORT(void*,cbHintAnimationPlugin_CreateDefault)()
{
	return (void*) new cbHintAnimationPlugin();
}
	
EWXWEXPORT(void*,cbHintAnimationPlugin_Create)(void* pPanel, int paneMask)
{
	return (void*) new cbHintAnimationPlugin((wxFrameLayout*)pPanel, paneMask);
}
	
EWXWEXPORT(void,cbHintAnimationPlugin_Delete)(void* _obj)
{
	delete (cbHintAnimationPlugin*)_obj;
}

EWXWEXPORT(void*,wxNewBitmapButton_Create)(void* labelBitmap, void* labelText, int alignText, int isFlat, int firedEventType, int marginX, int marginY, int textToLabelGap, int isSticky)
{
	return (void*) new wxNewBitmapButton(*((wxBitmap*)labelBitmap), (char*)labelText, alignText, isFlat != 0, firedEventType, marginX, marginY, textToLabelGap, isSticky != 0);
}
	
EWXWEXPORT(void*,wxNewBitmapButton_CreateFromFile)(void* bitmapFileName, int bitmapFileType, void* labelText, int alignText, int isFlat, int firedEventType, int marginX, int marginY, int textToLabelGap, int isSticky)
{
	return (void*) new wxNewBitmapButton((char*)bitmapFileName, (wxBitmapType)bitmapFileType, (char*)labelText, alignText, isFlat != 0, firedEventType, marginX, marginY, textToLabelGap, isSticky != 0);
}

EWXWEXPORT(void,wxNewBitmapButton_Realize)(void* _obj, void* _prt, int _id, int _x, int _y, int _w, int _h)
{
	((wxNewBitmapButton*)_obj)->Create((wxWindow*)_prt, (wxWindowID)_id, wxPoint(_x, _y), wxSize(_w, _h));
}

EWXWEXPORT(void,wxNewBitmapButton_Delete)(void* _obj)
{
	delete (wxNewBitmapButton*)_obj;
}

EWXWEXPORT(void,wxNewBitmapButton_Reshape)(void* _obj)
{
	((wxNewBitmapButton*)_obj)->Reshape();
}
	
EWXWEXPORT(void,wxNewBitmapButton_SetLabel)(void* _obj, void* labelBitmap, void* labelText)
{
	((wxNewBitmapButton*)_obj)->SetLabel(*((wxBitmap*)labelBitmap), (char*)labelText);
}
	
EWXWEXPORT(void,wxNewBitmapButton_SetAlignments)(void* _obj, int alignText, int marginX, int marginY, int textToLabelGap)
{
	((wxNewBitmapButton*)_obj)->SetAlignments(alignText, marginX, marginY, textToLabelGap);
}
	
EWXWEXPORT(void,wxNewBitmapButton_DrawDecorations)(void* _obj, void* dc)
{
	((wxNewBitmapButton*)_obj)->DrawDecorations(*((wxDC*)dc));
}
	
EWXWEXPORT(void,wxNewBitmapButton_DrawLabel)(void* _obj, void* dc)
{
	((wxNewBitmapButton*)_obj)->DrawLabel(*((wxDC*)dc));
}
	
EWXWEXPORT(void,wxNewBitmapButton_RenderLabelImage)(void* _obj, void* destBmp, void* srcBmp, int isEnabled, int isPressed)
{
	((wxNewBitmapButton*)_obj)->RenderLabelImage((wxBitmap*)destBmp, (wxBitmap*)srcBmp, isEnabled != 0, isPressed != 0);
}
	
EWXWEXPORT(void,wxNewBitmapButton_RenderLabelImages)(void* _obj)
{
	((wxNewBitmapButton*)_obj)->RenderLabelImages();
}
	
EWXWEXPORT(void,wxNewBitmapButton_RenderAllLabelImages)(void* _obj)
{
	((wxNewBitmapButton*)_obj)->RenderAllLabelImages();
}
	
EWXWEXPORT(int,wxNewBitmapButton_Enable)(void* _obj, int enable)
{
	return (int)((wxNewBitmapButton*)_obj)->Enable(enable != 0);
}
	
EWXWEXPORT(void*,cbPaneDrawPlugin_CreateDefault)()
{
	return (void*) new cbPaneDrawPlugin();
}
	
EWXWEXPORT(void*,cbPaneDrawPlugin_Create)(void* pPanel, int paneMask)
{
	return (void*) new cbPaneDrawPlugin((wxFrameLayout*)pPanel, paneMask);
}
	
EWXWEXPORT(void,cbPaneDrawPlugin_Delete)(void* _obj)
{
	delete (cbPaneDrawPlugin*)_obj;
}

EWXWEXPORT(void*,cbRowDragPlugin_CreateDefault)()
{
	return (void*) new cbRowDragPlugin();
}
	
EWXWEXPORT(void*,cbRowDragPlugin_Create)(void* pPanel, int paneMask)
{
	return (void*) new cbRowDragPlugin((wxFrameLayout*)pPanel, paneMask);
}
	
EWXWEXPORT(void,cbRowDragPlugin_Delete)(void* _obj)
{
	delete (cbRowDragPlugin*)_obj;
}

EWXWEXPORT(void*,cbRowLayoutPlugin_CreateDefault)()
{
	return (void*) new cbRowLayoutPlugin();
}
	
EWXWEXPORT(void*,cbRowLayoutPlugin_Create)(void* pPanel, int paneMask)
{
	return (void*) new cbRowLayoutPlugin((wxFrameLayout*)pPanel, paneMask);
}
	
EWXWEXPORT(void,cbRowLayoutPlugin_Delete)(void* _obj)
{
	delete (cbRowLayoutPlugin*)_obj;
}

EWXWEXPORT(void*,cbPluginBase_Plugin)(int _swt)
{
	switch (_swt)
	{
		case 0:
			return CLASSINFO(cbAntiflickerPlugin);
		
		case 1:
			return CLASSINFO(cbBarDragPlugin);
		
		case 2:
			return CLASSINFO(cbBarHintsPlugin);
		
		case 3:
			return CLASSINFO(cbSimpleCustomizationPlugin);
		
		case 4:
			return CLASSINFO(cbHintAnimationPlugin);
		
		case 5:
			return CLASSINFO(cbPaneDrawPlugin);
		
		case 6:
			return CLASSINFO(cbRowDragPlugin);
		
		case 7:
			return CLASSINFO(cbRowLayoutPlugin);
		
		default:
			return NULL;
	}
}

EWXWEXPORT(void*,cbCommonPaneProperties_CreateDefault)()
{
	return (void*)new cbCommonPaneProperties();
}
	
EWXWEXPORT(void,cbCommonPaneProperties_Delete)(void* _obj)
{
	delete (cbCommonPaneProperties*)_obj;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_RealTimeUpdatesOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mRealTimeUpdatesOn;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_OutOfPaneDragOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mOutOfPaneDragOn;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_ExactDockPredictionOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mExactDockPredictionOn;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_NonDestructFrictionOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mNonDestructFrictionOn;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_Show3DPaneBorderOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mShow3DPaneBorderOn;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_BarFloatingOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mBarFloatingOn;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_RowProportionsOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mRowProportionsOn;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_ColProportionsOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mColProportionsOn;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_BarCollapseIconsOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mBarCollapseIconsOn;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_BarDragHintsOn)(void* _obj)
{
	return (int)((cbCommonPaneProperties*)_obj)->mBarDragHintsOn;
}
	
EWXWEXPORT(void,cbCommonPaneProperties_MinCBarDim)(void* _obj, void* _w, void* _h)
{
	wxSize size = ((cbCommonPaneProperties*)_obj)->mMinCBarDim;
	*((int*)_w) = size.x;
	*((int*)_h) = size.y;
}
	
EWXWEXPORT(int,cbCommonPaneProperties_ResizeHandleSize)(void* _obj)
{
	return ((cbCommonPaneProperties*)_obj)->mResizeHandleSize;
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetRealTimeUpdatesOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mRealTimeUpdatesOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetOutOfPaneDragOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mOutOfPaneDragOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetExactDockPredictionOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mExactDockPredictionOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetNonDestructFrictionOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mNonDestructFrictionOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetShow3DPaneBorderOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mShow3DPaneBorderOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetBarFloatingOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mBarFloatingOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetRowProportionsOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mRowProportionsOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetColProportionsOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mColProportionsOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetBarCollapseIconsOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mBarCollapseIconsOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetBarDragHintsOn)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mBarDragHintsOn = (_val != 0);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetMinCBarDim)(void* _obj, int _w, int _h)
{
	((cbCommonPaneProperties*)_obj)->mMinCBarDim = wxSize (_w, _h);
}
	
EWXWEXPORT(void,cbCommonPaneProperties_SetResizeHandleSize)(void* _obj, int _val)
{
	((cbCommonPaneProperties*)_obj)->mResizeHandleSize = _val;
}
	
EWXWEXPORT(void,cbCommonPaneProperties_Assign)(void* _obj, void* _other)
{
	*((cbCommonPaneProperties*)_obj) = *((cbCommonPaneProperties*)_other);
}
	
}
