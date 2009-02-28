#include "wrapper.h"
#include "wx/process.h"
#include "wx/dialup.h"
#include "wx/tabctrl.h"

#if (wxVERSION_NUMBER >= 2800)
#include "wx/power.h"
#endif

#if defined(wxUSE_TAB_DIALOG) && (wxUSE_TAB_DIALOG==0)
# undef wxUSE_TAB_DIALOG
#endif

#ifdef USE_CONTRIB
#include "wx/plot/plot.h"
#include "wx/gizmos/dynamicsash.h"
#endif

extern "C"
{

EWXWEXPORT(wxCommandEvent*,wxCommandEvent_Create)(int _typ,int _id)
{
        return new wxCommandEvent((wxEventType)_typ, _id);
}

EWXWEXPORT(void,wxCommandEvent_Delete)(wxCommandEvent* self)
{
        delete self;
}

EWXWEXPORT(int,wxEvent_GetTimestamp)(wxEvent* self)
{
        return self->GetTimestamp();
}

EWXWEXPORT(void,wxEvent_Skip)(wxEvent* self)
{
        self->Skip();
}

EWXWEXPORT(int,wxEvent_GetEventType)(wxEvent* self)
{
        return (int)self->GetEventType();
}

EWXWEXPORT(void,wxEvent_SetEventType)(wxEvent* self,int typ)
{
        self->SetEventType((wxEventType) typ);
}

EWXWEXPORT(void*,wxEvent_GetEventObject)(wxEvent* self)
{
        return (void*)self->GetEventObject();
}

EWXWEXPORT(void,wxEvent_SetEventObject)(wxEvent* self,wxObject* obj)
{
        self->SetEventObject(obj);
}

EWXWEXPORT(void,wxEvent_SetTimestamp)(wxEvent* self,int ts)
{
        self->SetTimestamp((long)ts);
}

EWXWEXPORT(int,wxEvent_GetId)(wxEvent* self)
{
        return self->GetId();
}

EWXWEXPORT(void,wxEvent_SetId)(wxEvent* self,int Id)
{
        self->SetId(Id);
}

EWXWEXPORT(bool,wxEvent_GetSkipped)(wxEvent* self)
{
        return self->GetSkipped();
}

EWXWEXPORT(bool,wxEvent_IsCommandEvent)(wxEvent* self)
{
        return self->IsCommandEvent();
}

EWXWEXPORT(void,wxEvent_CopyObject)(wxEvent* self,wxObject* object_dest)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*object_dest);
#endif
}

EWXWEXPORT(void,wxEvent_SetCallbackUserData)(wxEvent* self,wxObject* obj)
{
        self->m_callbackUserData = obj;
}

EWXWEXPORT(void*,wxEvent_GetCallbackUserData)(wxEvent* self)
{
        return (void*)self->m_callbackUserData;
}

EWXWEXPORT(void,wxCommandEvent_SetClientData)(wxCommandEvent* self,void* clientData)
{
        self->SetClientData(clientData);
}

EWXWEXPORT(void*,wxCommandEvent_GetClientData)(wxCommandEvent* self)
{
        return self->GetClientData();
}

EWXWEXPORT(void,wxCommandEvent_SetClientObject)(wxCommandEvent* self,void* clientObject)
{
        self->SetClientObject((wxClientData*)clientObject);
}

EWXWEXPORT(void*,wxCommandEvent_GetClientObject)(wxCommandEvent* self)
{
        return (void*)self->GetClientObject();
}

EWXWEXPORT(int,wxCommandEvent_GetSelection)(wxCommandEvent* self)
{
        return self->GetSelection();
}

EWXWEXPORT(void,wxCommandEvent_SetString)(wxCommandEvent* self,wxString* s)
{
        self->SetString(*s);
}

EWXWEXPORT(wxString*,wxCommandEvent_GetString)(wxCommandEvent* self)
{
        wxString *result = new wxString();
        *result = self->GetString();
        return result;
}

EWXWEXPORT(bool,wxCommandEvent_IsChecked)(wxCommandEvent* self)
{
        return self->IsChecked();
}

EWXWEXPORT(bool,wxCommandEvent_IsSelection)(wxCommandEvent* self)
{
        return self->IsSelection();
}

EWXWEXPORT(void,wxCommandEvent_SetExtraLong)(wxCommandEvent* self,long extraLong)
{
        self->SetExtraLong(extraLong);
}

EWXWEXPORT(long,wxCommandEvent_GetExtraLong)(wxCommandEvent* self)
{
        return self->GetExtraLong();
}

EWXWEXPORT(void,wxCommandEvent_SetInt)(wxCommandEvent* self,int i)
{
        self->SetInt(i);
}

EWXWEXPORT(long,wxCommandEvent_GetInt)(wxCommandEvent* self)
{
        return self->GetInt();
}

EWXWEXPORT(void,wxCommandEvent_CopyObject)(wxCommandEvent* self,wxObject* object_dest)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*object_dest);
#endif
}

EWXWEXPORT(void,wxNotifyEvent_Veto)(wxNotifyEvent* self)
{
        self->Veto();
}

EWXWEXPORT(void,wxNotifyEvent_Allow)(wxNotifyEvent* self)
{
        self->Allow();
}

EWXWEXPORT(bool,wxNotifyEvent_IsAllowed)(wxNotifyEvent* self)
{
        return self->IsAllowed();
}

EWXWEXPORT(void,wxNotifyEvent_CopyObject)(wxNotifyEvent* self,wxObject* object_dest)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*object_dest);
#endif
}

EWXWEXPORT(int,wxScrollWinEvent_GetOrientation)(wxScrollWinEvent* self)
{
        return self->GetOrientation();
}

EWXWEXPORT(int,wxScrollWinEvent_GetPosition)(wxScrollWinEvent* self)
{
        return self->GetPosition();
}

EWXWEXPORT(void,wxScrollWinEvent_SetOrientation)(wxScrollWinEvent* self,int orient)
{
        self->SetOrientation(orient);
}

EWXWEXPORT(void,wxScrollWinEvent_SetPosition)(wxScrollWinEvent* self,int pos)
{
        self->SetPosition(pos);
}

EWXWEXPORT(bool,wxMouseEvent_IsButton)(wxMouseEvent* self)
{
        return self->IsButton();
}

EWXWEXPORT(bool,wxMouseEvent_ButtonDown)(wxMouseEvent* self,int but)
{
        return self->ButtonDown(but);
}

EWXWEXPORT(bool,wxMouseEvent_ButtonDClick)(wxMouseEvent* self,int but)
{
        return self->ButtonDClick(but);
}

EWXWEXPORT(bool,wxMouseEvent_ButtonUp)(wxMouseEvent* self,int but)
{
        return self->ButtonUp(but);
}

EWXWEXPORT(bool,wxMouseEvent_Button)(wxMouseEvent* self,int but)
{
        return self->Button(but);
}

EWXWEXPORT(bool,wxMouseEvent_ButtonIsDown)(wxMouseEvent* self,int but)
{
        return self->ButtonIsDown(but);
}

EWXWEXPORT(bool,wxMouseEvent_ControlDown)(wxMouseEvent* self)
{
        return self->ControlDown();
}

EWXWEXPORT(bool,wxMouseEvent_MetaDown)(wxMouseEvent* self)
{
        return self->MetaDown();
}

EWXWEXPORT(bool,wxMouseEvent_AltDown)(wxMouseEvent* self)
{
        return self->AltDown();
}

EWXWEXPORT(bool,wxMouseEvent_ShiftDown)(wxMouseEvent* self)
{
        return self->ShiftDown();
}

EWXWEXPORT(bool,wxMouseEvent_LeftDown)(wxMouseEvent* self)
{
        return self->LeftDown();
}

EWXWEXPORT(bool,wxMouseEvent_MiddleDown)(wxMouseEvent* self)
{
        return self->MiddleDown();
}

EWXWEXPORT(bool,wxMouseEvent_RightDown)(wxMouseEvent* self)
{
        return self->RightDown();
}

EWXWEXPORT(bool,wxMouseEvent_LeftUp)(wxMouseEvent* self)
{
        return self->LeftUp();
}

EWXWEXPORT(bool,wxMouseEvent_MiddleUp)(wxMouseEvent* self)
{
        return self->MiddleUp();
}

EWXWEXPORT(bool,wxMouseEvent_RightUp)(wxMouseEvent* self)
{
        return self->RightUp();
}

EWXWEXPORT(bool,wxMouseEvent_LeftDClick)(wxMouseEvent* self)
{
        return self->LeftDClick();
}

EWXWEXPORT(bool,wxMouseEvent_MiddleDClick)(wxMouseEvent* self)
{
        return self->MiddleDClick();
}

EWXWEXPORT(bool,wxMouseEvent_RightDClick)(wxMouseEvent* self)
{
        return self->RightDClick();
}

EWXWEXPORT(bool,wxMouseEvent_LeftIsDown)(wxMouseEvent* self)
{
        return self->LeftIsDown();
}

EWXWEXPORT(bool,wxMouseEvent_MiddleIsDown)(wxMouseEvent* self)
{
        return self->MiddleIsDown();
}

EWXWEXPORT(bool,wxMouseEvent_RightIsDown)(wxMouseEvent* self)
{
        return self->RightIsDown();
}

EWXWEXPORT(bool,wxMouseEvent_Dragging)(wxMouseEvent* self)
{
        return self->Dragging();
}

EWXWEXPORT(bool,wxMouseEvent_Moving)(wxMouseEvent* self)
{
        return self->Moving();
}

EWXWEXPORT(bool,wxMouseEvent_Entering)(wxMouseEvent* self)
{
        return self->Entering();
}

EWXWEXPORT(bool,wxMouseEvent_Leaving)(wxMouseEvent* self)
{
        return self->Leaving();
}

EWXWEXPORT(void, wxMouseEvent_GetPosition)(void* _obj, int* xpos, int* ypos)
{
        ((wxMouseEvent*)_obj)->GetPosition((wxCoord*)xpos, (wxCoord*)ypos);
}

EWXWEXPORT(void, wxMouseEvent_GetLogicalPosition)(void* _obj, void* dc, int* xpos, int* ypos)
{
        wxPoint pt = ((wxMouseEvent*)_obj)->GetLogicalPosition(*((wxDC*)dc));
        *xpos = pt.x;
        *ypos = pt.y;
}

EWXWEXPORT(int,wxMouseEvent_GetX)(wxMouseEvent* self)
{
        return self->GetX();
}

EWXWEXPORT(int,wxMouseEvent_GetY)(wxMouseEvent* self)
{
        return self->GetY();
}

EWXWEXPORT(void,wxMouseEvent_CopyObject)(wxMouseEvent* self,wxObject* object_dest)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*object_dest);
#endif
}

EWXWEXPORT(wxCoord,wxSetCursorEvent_GetX)(wxSetCursorEvent* self)
{
        return self->GetX();
}

EWXWEXPORT(wxCoord,wxSetCursorEvent_GetY)(wxSetCursorEvent* self)
{
        return self->GetY();
}

EWXWEXPORT(void,wxSetCursorEvent_SetCursor)(wxSetCursorEvent* self,wxCursor* cursor)
{
        self->SetCursor(*cursor);
}

EWXWEXPORT(void*,wxSetCursorEvent_GetCursor)(void* self)
{
        return (void*)(&((wxSetCursorEvent*)self)->GetCursor());
}

EWXWEXPORT(bool,wxSetCursorEvent_HasCursor)(wxSetCursorEvent* self)
{
        return self->HasCursor();
}

EWXWEXPORT(bool,wxKeyEvent_ControlDown)(wxKeyEvent* self)
{
        return self->ControlDown();
}

EWXWEXPORT(bool,wxKeyEvent_MetaDown)(wxKeyEvent* self)
{
        return self->MetaDown();
}

EWXWEXPORT(bool,wxKeyEvent_AltDown)(wxKeyEvent* self)
{
        return self->AltDown();
}

EWXWEXPORT(bool,wxKeyEvent_ShiftDown)(wxKeyEvent* self)
{
        return self->ShiftDown();
}

EWXWEXPORT(bool,wxKeyEvent_HasModifiers)(wxKeyEvent* self)
{
        return self->HasModifiers();
}

EWXWEXPORT(int,wxKeyEvent_GetKeyCode)(wxKeyEvent* self)
{
        return self->GetKeyCode();
}

EWXWEXPORT(int,wxKeyEvent_GetModifiers)(wxKeyEvent* self)
{
        return self->GetModifiers();
}

EWXWEXPORT(void,wxKeyEvent_SetKeyCode)(wxKeyEvent* self,int code)
{
        self->m_keyCode = code;
}

EWXWEXPORT(void, wxKeyEvent_GetPosition)(void* _obj, int* xpos, int* ypos)
{
        ((wxKeyEvent*)_obj)->GetPosition((wxCoord*) xpos, (wxCoord*) ypos);
}

EWXWEXPORT(wxCoord,wxKeyEvent_GetX)(wxKeyEvent* self)
{
        return self->GetX();
}

EWXWEXPORT(wxCoord,wxKeyEvent_GetY)(wxKeyEvent* self)
{
        return self->GetY();
}

EWXWEXPORT(void,wxKeyEvent_CopyObject)(wxKeyEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(void, wxSizeEvent_GetSize)(void* _obj, int* w, int* h)
{
        wxSize sz = ((wxSizeEvent*)_obj)->GetSize();
        *w = sz.x;
        *h = sz.y;
}

EWXWEXPORT(void,wxSizeEvent_CopyObject)(wxSizeEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(void, wxMoveEvent_GetPosition)(void* _obj, int* x, int* y)
{
        wxPoint pt = ((wxMoveEvent*)_obj)->GetPosition();
        *x = pt.x;
        *y = pt.y;
}

EWXWEXPORT(void,wxMoveEvent_CopyObject)(wxMoveEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(void*,wxEraseEvent_GetDC)(void* self)
{
        return (void*)((wxEraseEvent*)self)->GetDC();
}

EWXWEXPORT(void,wxEraseEvent_CopyObject)(wxEraseEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(bool,wxActivateEvent_GetActive)(wxActivateEvent* self)
{
        return self->GetActive();
}

EWXWEXPORT(void,wxActivateEvent_CopyObject)(wxActivateEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(int,wxMenuEvent_GetMenuId)(wxMenuEvent* self)
{
        return self->GetMenuId();
}

EWXWEXPORT(void,wxMenuEvent_CopyObject)(wxMenuEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(void,wxCloseEvent_SetLoggingOff)(wxCloseEvent* self,bool logOff)
{
        self->SetLoggingOff(logOff);
}

EWXWEXPORT(bool,wxCloseEvent_GetLoggingOff)(wxCloseEvent* self)
{
        return self->GetLoggingOff();
}

EWXWEXPORT(void,wxCloseEvent_Veto)(wxCloseEvent* self,bool veto)
{
        self->Veto(veto);
}

EWXWEXPORT(void,wxCloseEvent_SetCanVeto)(wxCloseEvent* self,bool canVeto)
{
        self->SetCanVeto(canVeto);
}

EWXWEXPORT(bool,wxCloseEvent_CanVeto)(wxCloseEvent* self)
{
        return self->CanVeto();
}

EWXWEXPORT(bool,wxCloseEvent_GetVeto)(wxCloseEvent* self)
{
        return self->GetVeto();
}

EWXWEXPORT(void,wxCloseEvent_CopyObject)(wxCloseEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(void,wxShowEvent_SetShow)(wxShowEvent* self,bool show)
{
        self->SetShow(show);
}

EWXWEXPORT(bool,wxShowEvent_GetShow)(wxShowEvent* self)
{
        return self->GetShow();
}

EWXWEXPORT(void,wxShowEvent_CopyObject)(wxShowEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(void, wxJoystickEvent_GetPosition)(void* _obj, int* x, int* y)
{
        wxPoint pt = ((wxJoystickEvent*)_obj)->GetPosition();
        *x = pt.x;
        *y = pt.y;
}

EWXWEXPORT(int,wxJoystickEvent_GetZPosition)(wxJoystickEvent* self)
{
        return self->GetZPosition();
}

EWXWEXPORT(int,wxJoystickEvent_GetButtonState)(wxJoystickEvent* self)
{
        return self->GetButtonState();
}

EWXWEXPORT(int,wxJoystickEvent_GetButtonChange)(wxJoystickEvent* self)
{
        return self->GetButtonChange();
}

EWXWEXPORT(int,wxJoystickEvent_GetJoystick)(wxJoystickEvent* self)
{
        return self->GetJoystick();
}

EWXWEXPORT(void,wxJoystickEvent_SetJoystick)(wxJoystickEvent* self,int stick)
{
        self->SetJoystick(stick);
}

EWXWEXPORT(void,wxJoystickEvent_SetButtonState)(wxJoystickEvent* self,int state)
{
        self->SetButtonState(state);
}

EWXWEXPORT(void,wxJoystickEvent_SetButtonChange)(wxJoystickEvent* self,int change)
{
        self->SetButtonChange(change);
}

EWXWEXPORT(void,wxJoystickEvent_SetPosition)(wxJoystickEvent* self,void* pos)
{
        self->SetPosition(*((wxPoint*)pos));
}

EWXWEXPORT(void,wxJoystickEvent_SetZPosition)(wxJoystickEvent* self,int zPos)
{
        self->SetZPosition(zPos);
}

EWXWEXPORT(bool,wxJoystickEvent_IsButton)(wxJoystickEvent* self)
{
        return self->IsButton();
}

EWXWEXPORT(bool,wxJoystickEvent_IsMove)(wxJoystickEvent* self)
{
        return self->IsMove();
}

EWXWEXPORT(bool,wxJoystickEvent_IsZMove)(wxJoystickEvent* self)
{
        return self->IsZMove();
}

EWXWEXPORT(bool,wxJoystickEvent_ButtonDown)(wxJoystickEvent* self,int but)
{
        return self->ButtonDown(but);
}

EWXWEXPORT(bool,wxJoystickEvent_ButtonUp)(wxJoystickEvent* self,int but)
{
        return self->ButtonUp(but);
}

EWXWEXPORT(bool,wxJoystickEvent_ButtonIsDown)(wxJoystickEvent* self,int but)
{
        return self->ButtonIsDown(but);
}

EWXWEXPORT(void,wxJoystickEvent_CopyObject)(wxJoystickEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(bool,wxUpdateUIEvent_GetChecked)(wxUpdateUIEvent* self)
{
        return self->GetChecked();
}

EWXWEXPORT(bool,wxUpdateUIEvent_GetEnabled)(wxUpdateUIEvent* self)
{
        return self->GetEnabled();
}

EWXWEXPORT(wxString*,wxUpdateUIEvent_GetText)(wxUpdateUIEvent* self)
{
        wxString *result = new wxString();
        *result = self->GetText();
        return result;
}

EWXWEXPORT(bool,wxUpdateUIEvent_GetSetText)(wxUpdateUIEvent* self)
{
        return self->GetSetText();
}

EWXWEXPORT(bool,wxUpdateUIEvent_GetSetChecked)(wxUpdateUIEvent* self)
{
        return self->GetSetChecked();
}

EWXWEXPORT(bool,wxUpdateUIEvent_GetSetEnabled)(wxUpdateUIEvent* self)
{
        return self->GetSetEnabled();
}

EWXWEXPORT(void,wxUpdateUIEvent_Check)(wxUpdateUIEvent* self,bool check)
{
        self->Check(check);
}

EWXWEXPORT(void,wxUpdateUIEvent_Enable)(wxUpdateUIEvent* self,bool enable)
{
        self->Enable(enable);
}

EWXWEXPORT(void,wxUpdateUIEvent_SetText)(wxUpdateUIEvent* self,wxString* text)
{
        self->SetText(*text);
}

EWXWEXPORT(void,wxUpdateUIEvent_CopyObject)(wxUpdateUIEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(void,wxPaletteChangedEvent_SetChangedWindow)(wxPaletteChangedEvent* self,wxWindow* win)
{
        self->SetChangedWindow(win);
}

EWXWEXPORT(void*,wxPaletteChangedEvent_GetChangedWindow)(wxPaletteChangedEvent* self)
{
        return (void*)self->GetChangedWindow();
}

EWXWEXPORT(void,wxPaletteChangedEvent_CopyObject)(wxPaletteChangedEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(void,wxQueryNewPaletteEvent_SetPaletteRealized)(wxQueryNewPaletteEvent* self,bool realized)
{
        self->SetPaletteRealized(realized);
}

EWXWEXPORT(bool,wxQueryNewPaletteEvent_GetPaletteRealized)(wxQueryNewPaletteEvent* self)
{
        return self->GetPaletteRealized();
}

EWXWEXPORT(void,wxQueryNewPaletteEvent_CopyObject)(wxQueryNewPaletteEvent* self,wxObject* obj)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*obj);
#endif
}

EWXWEXPORT(bool,wxNavigationKeyEvent_GetDirection)(wxNavigationKeyEvent* self)
{
        return self->GetDirection();
}

EWXWEXPORT(void,wxNavigationKeyEvent_SetDirection)(wxNavigationKeyEvent* self,bool bForward)
{
        self->SetDirection(bForward);
}

EWXWEXPORT(bool,wxNavigationKeyEvent_IsWindowChange)(wxNavigationKeyEvent* self)
{
        return self->IsWindowChange();
}

EWXWEXPORT(void,wxNavigationKeyEvent_SetWindowChange)(wxNavigationKeyEvent* self,bool bIs)
{
        self->SetWindowChange(bIs);
}

EWXWEXPORT(bool,wxNavigationKeyEvent_ShouldPropagate)(wxNavigationKeyEvent* self)
{
        return self->ShouldPropagate();
}
	
EWXWEXPORT(void*,wxNavigationKeyEvent_GetCurrentFocus)(wxNavigationKeyEvent* self)
{
        return (void*) self->GetCurrentFocus();
}

EWXWEXPORT(void,wxNavigationKeyEvent_SetCurrentFocus)(wxNavigationKeyEvent* self,wxWindow* win)
{
        self->SetCurrentFocus(win);
}

EWXWEXPORT(void*,wxWindowCreateEvent_GetWindow)(wxWindowCreateEvent* self)
{
        return (void*)self->GetWindow();
}

EWXWEXPORT(void*,wxWindowDestroyEvent_GetWindow)(wxWindowDestroyEvent* self)
{
        return (void*)self->GetWindow();
}

EWXWEXPORT(void,wxIdleEvent_RequestMore)(wxIdleEvent* self,bool needMore)
{
        self->RequestMore(needMore);
}

EWXWEXPORT(bool,wxIdleEvent_MoreRequested)(wxIdleEvent* self)
{
        return self->MoreRequested();
}

EWXWEXPORT(void,wxIdleEvent_CopyObject)(wxIdleEvent* self,wxObject* object_dest)
{
#if wxVERSION_NUMBER < 2400
        self->CopyObject(*object_dest);
#endif
}

EWXWEXPORT(int,wxListEvent_GetCode)(wxListEvent* self)
{
#if wxCHECK_VERSION(2,5,0)
	return self->GetKeyCode();
#else
        return self->GetCode();
#endif
}

EWXWEXPORT(long,wxListEvent_GetIndex)(wxListEvent* self)
{
        return self->GetIndex();
}
EWXWEXPORT(int,wxListEvent_GetColumn)(wxListEvent* self)
{
        return self->GetColumn();
}

EWXWEXPORT(bool,wxListEvent_Cancelled)(wxListEvent* self)
{
#if wxVERSION_NUMBER < 2400
        return self->Cancelled();
#else
        return false;
#endif
}

EWXWEXPORT(void, wxListEvent_GetPoint)(void* _obj, void* x, void* y)
{
        wxPoint pos = ((wxListEvent*)_obj)->GetPoint();
        *((int*)x) = pos.x;
        *((int*)y) = pos.y;
}

EWXWEXPORT(wxString*,wxListEvent_GetLabel)(wxListEvent* self)
{
        wxString *result = new wxString();
        *result = self->GetLabel();
        return result;
}

EWXWEXPORT(wxString*,wxListEvent_GetText)(wxListEvent* self)
{
        wxString *result = new wxString();
        *result = self->GetText();
        return result;
}

EWXWEXPORT(int,wxListEvent_GetImage)(wxListEvent* self)
{
        return self->GetImage();
}

EWXWEXPORT(long,wxListEvent_GetData)(wxListEvent* self)
{
        return self->GetData();
}

EWXWEXPORT(long,wxListEvent_GetMask)(wxListEvent* self)
{
        return self->GetMask();
}

EWXWEXPORT(void,wxListEvent_GetItem)(wxListEvent* self,void* _ref)
{
#if wxVERSION_NUMBER < 2400
        *((wxListItem*)_ref) = self->GetItem();
#else
        wxListItem* ret = new wxListItem(self->GetItem());
        *((void**)_ref) = (void*)ret;
#endif
}

EWXWEXPORT(void,wxTreeEvent_GetItem)(wxTreeEvent* self,wxTreeItemId* _ref)
{
        *_ref = self->GetItem();
}

EWXWEXPORT(void,wxTreeEvent_GetOldItem)(wxTreeEvent* self,wxTreeItemId* _ref)
{
        *_ref = self->GetOldItem();
}

EWXWEXPORT(void, wxTreeEvent_GetPoint)(void* _obj, void* x, void* y)
{
        wxPoint pos = ((wxTreeEvent*)_obj)->GetPoint();
        *((int*)x) = pos.x;
        *((int*)y) = pos.y;
}

EWXWEXPORT(int,wxTreeEvent_GetCode)(wxTreeEvent* self)
{
        return self->GetKeyCode();
}

EWXWEXPORT(wxString*,wxTreeEvent_GetLabel)(wxTreeEvent* self)
{
        wxString *result = new wxString();
        *result = self->GetLabel();
        return result;
}

EWXWEXPORT(int,wxSpinEvent_GetPosition)(wxSpinEvent* self)
{
        return self->GetPosition();
}

EWXWEXPORT(void,wxSpinEvent_SetPosition)(wxSpinEvent* self,int pos)
{
        self->SetPosition(pos);
}

EWXWEXPORT(int,wxTimerEvent_GetInterval)(wxTimerEvent* self)
{
        return self->GetInterval();
}

EWXWEXPORT(int,wxCalendarEvent_GetWeekDay)(wxCalendarEvent* self)
{
        return self->GetWeekDay();
}

EWXWEXPORT(void,wxCalendarEvent_GetDate)(wxCalendarEvent* self,wxDateTime* _dte)
{
        *_dte = self->GetDate();
}


EWXWEXPORT(int,wxScrollEvent_GetOrientation)(wxScrollEvent* self)
{
        return self->GetOrientation();
}

EWXWEXPORT(int,wxScrollEvent_GetPosition)(wxScrollEvent* self)
{
        return self->GetPosition();
}

EWXWEXPORT(void,wxHelpEvent_GetPosition)(void* _obj, void* x, void* y)
{
        wxPoint pos = ((wxHelpEvent*)_obj)->GetPosition();
        *((int*)x) = pos.x;
        *((int*)y) = pos.y;
}

EWXWEXPORT(void,wxHelpEvent_SetPosition)(wxHelpEvent* self,int x,int y)
{
        self->SetPosition(wxPoint(x, y));
}

EWXWEXPORT(wxString*,wxHelpEvent_GetLink)(wxHelpEvent* self)
{
        wxString *result = new wxString();
        *result = self->GetLink();
        return result;
}

EWXWEXPORT(void,wxHelpEvent_SetLink)(wxHelpEvent* self,wxString* link)
{
        self->SetLink(*link);
}

EWXWEXPORT(wxString*,wxHelpEvent_GetTarget)(wxHelpEvent* self)
{
        wxString *result = new wxString();
        *result = self->GetTarget();
        return result;
}

EWXWEXPORT(void,wxHelpEvent_SetTarget)(wxHelpEvent* self,wxString* target)
{
        self->SetTarget(*target);
}

EWXWEXPORT(int,expEVT_COMMAND_BUTTON_CLICKED)()
{
        return (int)wxEVT_COMMAND_BUTTON_CLICKED;
}

EWXWEXPORT(int,expEVT_COMMAND_CHECKBOX_CLICKED)()
{
        return (int)wxEVT_COMMAND_CHECKBOX_CLICKED;
}

EWXWEXPORT(int,expEVT_COMMAND_CHOICE_SELECTED)()
{
        return (int)wxEVT_COMMAND_CHOICE_SELECTED;
}

EWXWEXPORT(int,expEVT_COMMAND_LISTBOX_SELECTED)()
{
        return (int)wxEVT_COMMAND_LISTBOX_SELECTED;
}

EWXWEXPORT(int,expEVT_COMMAND_LISTBOX_DOUBLECLICKED)()
{
        return (int)wxEVT_COMMAND_LISTBOX_DOUBLECLICKED;
}

EWXWEXPORT(int,expEVT_COMMAND_CHECKLISTBOX_TOGGLED)()
{
        return (int)wxEVT_COMMAND_CHECKLISTBOX_TOGGLED;
}

EWXWEXPORT(int,expEVT_COMMAND_TEXT_UPDATED)()
{
        return (int)wxEVT_COMMAND_TEXT_UPDATED;
}

EWXWEXPORT(int,expEVT_COMMAND_TEXT_ENTER)()
{
        return (int)wxEVT_COMMAND_TEXT_ENTER;
}

EWXWEXPORT(int,expEVT_COMMAND_MENU_SELECTED)()
{
        return (int)wxEVT_COMMAND_MENU_SELECTED;
}

EWXWEXPORT(int,expEVT_COMMAND_TOOL_CLICKED)()
{
        return (int)wxEVT_COMMAND_TOOL_CLICKED;
}

EWXWEXPORT(int,expEVT_COMMAND_SLIDER_UPDATED)()
{
        return (int)wxEVT_COMMAND_SLIDER_UPDATED;
}

EWXWEXPORT(int,expEVT_COMMAND_RADIOBOX_SELECTED)()
{
        return (int)wxEVT_COMMAND_RADIOBOX_SELECTED;
}

EWXWEXPORT(int,expEVT_COMMAND_RADIOBUTTON_SELECTED)()
{
        return (int)wxEVT_COMMAND_RADIOBUTTON_SELECTED;
}

EWXWEXPORT(int,expEVT_COMMAND_SCROLLBAR_UPDATED)()
{
        return (int)wxEVT_COMMAND_SCROLLBAR_UPDATED;
}

EWXWEXPORT(int,expEVT_COMMAND_VLBOX_SELECTED)()
{
        return (int)wxEVT_COMMAND_VLBOX_SELECTED;
}

EWXWEXPORT(int,expEVT_COMMAND_COMBOBOX_SELECTED)()
{
        return (int)wxEVT_COMMAND_COMBOBOX_SELECTED;
}

EWXWEXPORT(int,expEVT_COMMAND_TOOL_RCLICKED)()
{
        return (int)wxEVT_COMMAND_TOOL_RCLICKED;
}

EWXWEXPORT(int,expEVT_COMMAND_TOOL_ENTER)()
{
        return (int)wxEVT_COMMAND_TOOL_ENTER;
}

EWXWEXPORT(int,expEVT_COMMAND_SPINCTRL_UPDATED)()
{
        return (int)wxEVT_COMMAND_SPINCTRL_UPDATED;
}

EWXWEXPORT(int,expEVT_SOCKET)()
{
        return (int)wxEVT_SOCKET;
}

EWXWEXPORT(int,expEVT_TIMER )()
{
        return (int)wxEVT_TIMER ;
}

EWXWEXPORT(int,expEVT_LEFT_DOWN)()
{
        return (int)wxEVT_LEFT_DOWN;
}

EWXWEXPORT(int,expEVT_LEFT_UP)()
{
        return (int)wxEVT_LEFT_UP;
}

EWXWEXPORT(int,expEVT_MIDDLE_DOWN)()
{
        return (int)wxEVT_MIDDLE_DOWN;
}

EWXWEXPORT(int,expEVT_MIDDLE_UP)()
{
        return (int)wxEVT_MIDDLE_UP;
}

EWXWEXPORT(int,expEVT_RIGHT_DOWN)()
{
        return (int)wxEVT_RIGHT_DOWN;
}

EWXWEXPORT(int,expEVT_RIGHT_UP)()
{
        return (int)wxEVT_RIGHT_UP;
}

EWXWEXPORT(int,expEVT_MOTION)()
{
        return (int)wxEVT_MOTION;
}

EWXWEXPORT(int,expEVT_ENTER_WINDOW)()
{
        return (int)wxEVT_ENTER_WINDOW;
}

EWXWEXPORT(int,expEVT_LEAVE_WINDOW)()
{
        return (int)wxEVT_LEAVE_WINDOW;
}

EWXWEXPORT(int,expEVT_LEFT_DCLICK)()
{
        return (int)wxEVT_LEFT_DCLICK;
}

EWXWEXPORT(int,expEVT_MIDDLE_DCLICK)()
{
        return (int)wxEVT_MIDDLE_DCLICK;
}

EWXWEXPORT(int,expEVT_RIGHT_DCLICK)()
{
        return (int)wxEVT_RIGHT_DCLICK;
}

EWXWEXPORT(int,expEVT_SET_FOCUS)()
{
        return (int)wxEVT_SET_FOCUS;
}

EWXWEXPORT(int,expEVT_KILL_FOCUS)()
{
        return (int)wxEVT_KILL_FOCUS;
}

EWXWEXPORT(int,expEVT_NC_LEFT_DOWN)()
{
        return (int)wxEVT_NC_LEFT_DOWN;
}

EWXWEXPORT(int,expEVT_NC_LEFT_UP)()
{
        return (int)wxEVT_NC_LEFT_UP;
}

EWXWEXPORT(int,expEVT_NC_MIDDLE_DOWN)()
{
        return (int)wxEVT_NC_MIDDLE_DOWN;
}

EWXWEXPORT(int,expEVT_NC_MIDDLE_UP)()
{
        return (int)wxEVT_NC_MIDDLE_UP;
}

EWXWEXPORT(int,expEVT_NC_RIGHT_DOWN)()
{
        return (int)wxEVT_NC_RIGHT_DOWN;
}

EWXWEXPORT(int,expEVT_NC_RIGHT_UP)()
{
        return (int)wxEVT_NC_RIGHT_UP;
}

EWXWEXPORT(int,expEVT_NC_MOTION)()
{
        return (int)wxEVT_NC_MOTION;
}

EWXWEXPORT(int,expEVT_NC_ENTER_WINDOW)()
{
        return (int)wxEVT_NC_ENTER_WINDOW;
}

EWXWEXPORT(int,expEVT_NC_LEAVE_WINDOW)()
{
        return (int)wxEVT_NC_LEAVE_WINDOW;
}

EWXWEXPORT(int,expEVT_NC_LEFT_DCLICK)()
{
        return (int)wxEVT_NC_LEFT_DCLICK;
}

EWXWEXPORT(int,expEVT_NC_MIDDLE_DCLICK)()
{
        return (int)wxEVT_NC_MIDDLE_DCLICK;
}

EWXWEXPORT(int,expEVT_NC_RIGHT_DCLICK)()
{
        return (int)wxEVT_NC_RIGHT_DCLICK;
}

EWXWEXPORT(int,expEVT_CHAR)()
{
        return (int)wxEVT_CHAR;
}

EWXWEXPORT(int,expEVT_CHAR_HOOK)()
{
        return (int)wxEVT_CHAR_HOOK;
}

EWXWEXPORT(int,expEVT_NAVIGATION_KEY)()
{
        return (int)wxEVT_NAVIGATION_KEY;
}

EWXWEXPORT(int,expEVT_KEY_DOWN)()
{
        return (int)wxEVT_KEY_DOWN;
}

EWXWEXPORT(int,expEVT_KEY_UP)()
{
        return (int)wxEVT_KEY_UP;
}

EWXWEXPORT(int,expEVT_SET_CURSOR)()
{
        return (int)wxEVT_SET_CURSOR;
}

EWXWEXPORT(int,expEVT_SCROLL_TOP)()
{
        return (int)wxEVT_SCROLL_TOP;
}

EWXWEXPORT(int,expEVT_SCROLL_BOTTOM)()
{
        return (int)wxEVT_SCROLL_BOTTOM;
}

EWXWEXPORT(int,expEVT_SCROLL_LINEUP)()
{
        return (int)wxEVT_SCROLL_LINEUP;
}

EWXWEXPORT(int,expEVT_SCROLL_LINEDOWN)()
{
        return (int)wxEVT_SCROLL_LINEDOWN;
}

EWXWEXPORT(int,expEVT_SCROLL_PAGEUP)()
{
        return (int)wxEVT_SCROLL_PAGEUP;
}

EWXWEXPORT(int,expEVT_SCROLL_PAGEDOWN)()
{
        return (int)wxEVT_SCROLL_PAGEDOWN;
}

EWXWEXPORT(int,expEVT_SCROLL_THUMBTRACK)()
{
        return (int)wxEVT_SCROLL_THUMBTRACK;
}

EWXWEXPORT(int,expEVT_SCROLL_THUMBRELEASE)()
{
        return (int)wxEVT_SCROLL_THUMBRELEASE;
}

EWXWEXPORT(int,expEVT_SCROLLWIN_TOP)()
{
        return (int)wxEVT_SCROLLWIN_TOP;
}

EWXWEXPORT(int,expEVT_SCROLLWIN_BOTTOM)()
{
        return (int)wxEVT_SCROLLWIN_BOTTOM;
}

EWXWEXPORT(int,expEVT_SCROLLWIN_LINEUP)()
{
        return (int)wxEVT_SCROLLWIN_LINEUP;
}

EWXWEXPORT(int,expEVT_SCROLLWIN_LINEDOWN)()
{
        return (int)wxEVT_SCROLLWIN_LINEDOWN;
}

EWXWEXPORT(int,expEVT_SCROLLWIN_PAGEUP)()
{
        return (int)wxEVT_SCROLLWIN_PAGEUP;
}

EWXWEXPORT(int,expEVT_SCROLLWIN_PAGEDOWN)()
{
        return (int)wxEVT_SCROLLWIN_PAGEDOWN;
}

EWXWEXPORT(int,expEVT_SCROLLWIN_THUMBTRACK)()
{
        return (int)wxEVT_SCROLLWIN_THUMBTRACK;
}

EWXWEXPORT(int,expEVT_SCROLLWIN_THUMBRELEASE)()
{
        return (int)wxEVT_SCROLLWIN_THUMBRELEASE;
}

EWXWEXPORT(int,expEVT_SIZE)()
{
        return (int)wxEVT_SIZE;
}

EWXWEXPORT(int,expEVT_MOVE)()
{
        return (int)wxEVT_MOVE;
}

EWXWEXPORT(int,expEVT_CLOSE_WINDOW)()
{
        return (int)wxEVT_CLOSE_WINDOW;
}

EWXWEXPORT(int,expEVT_END_SESSION)()
{
        return (int)wxEVT_END_SESSION;
}

EWXWEXPORT(int,expEVT_QUERY_END_SESSION)()
{
        return (int)wxEVT_QUERY_END_SESSION;
}

EWXWEXPORT(int,expEVT_ACTIVATE_APP)()
{
        return (int)wxEVT_ACTIVATE_APP;
}

EWXWEXPORT(int,expEVT_POWER)()
{
#if (wxVERSION_NUMBER <= 2800)
	return (int)wxEVT_POWER;
#else
	return 1;
#endif
}

EWXWEXPORT(int,expEVT_POWER_SUSPENDING)()
{
#ifdef wxHAS_POWER_EVENTS
        return (int)wxEVT_POWER_SUSPENDING;
#else
	return 0;
#endif
}

EWXWEXPORT(int,expEVT_POWER_SUSPENDED)()
{
#ifdef wxHAS_POWER_EVENTS
        return (int)wxEVT_POWER_SUSPENDED;
#else
	return 0;
#endif
}

EWXWEXPORT(int,expEVT_POWER_SUSPEND_CANCEL)()
{
#ifdef wxHAS_POWER_EVENTS
        return (int)wxEVT_POWER_SUSPEND_CANCEL;
#else
	return 0;
#endif
}

EWXWEXPORT(int,expEVT_POWER_RESUME)()
{
#ifdef wxHAS_POWER_EVENTS
        return (int)wxEVT_POWER_RESUME;
#else
	return 0;
#endif
}

EWXWEXPORT(int,expEVT_ACTIVATE)()
{
        return (int)wxEVT_ACTIVATE;
}

EWXWEXPORT(int,expEVT_CREATE)()
{
        return (int)wxEVT_CREATE;
}

EWXWEXPORT(int,expEVT_DESTROY)()
{
        return (int)wxEVT_DESTROY;
}

EWXWEXPORT(int,expEVT_SHOW)()
{
        return (int)wxEVT_SHOW;
}

EWXWEXPORT(int,expEVT_ICONIZE)()
{
        return (int)wxEVT_ICONIZE;
}

EWXWEXPORT(int,expEVT_MAXIMIZE)()
{
        return (int)wxEVT_MAXIMIZE;
}

EWXWEXPORT(int,expEVT_MOUSE_CAPTURE_CHANGED)()
{
        return (int)wxEVT_MOUSE_CAPTURE_CHANGED;
}

EWXWEXPORT(int,expEVT_PAINT)()
{
        return (int)wxEVT_PAINT;
}

EWXWEXPORT(int,expEVT_ERASE_BACKGROUND)()
{
        return (int)wxEVT_ERASE_BACKGROUND;
}

EWXWEXPORT(int,expEVT_NC_PAINT)()
{
        return (int)wxEVT_NC_PAINT;
}

EWXWEXPORT(int,expEVT_PAINT_ICON)()
{
        return (int)wxEVT_PAINT_ICON;
}

EWXWEXPORT(int,expEVT_MENU_CHAR)()
{
        return -1;
}

EWXWEXPORT(int,expEVT_MENU_INIT)()
{
        return -1;
}

EWXWEXPORT(int,expEVT_MENU_HIGHLIGHT)()
{
        return (int)wxEVT_MENU_HIGHLIGHT;
}

EWXWEXPORT(int,expEVT_POPUP_MENU_INIT)()
{
        return -1;
}

EWXWEXPORT(int,expEVT_CONTEXT_MENU)()
{
        return (int)wxEVT_CONTEXT_MENU;
}

EWXWEXPORT(int,expEVT_SYS_COLOUR_CHANGED)()
{
        return (int)wxEVT_SYS_COLOUR_CHANGED;
}

EWXWEXPORT(int,expEVT_SETTING_CHANGED)()
{
        return (int)wxEVT_SETTING_CHANGED;
}

EWXWEXPORT(int,expEVT_QUERY_NEW_PALETTE)()
{
        return (int)wxEVT_QUERY_NEW_PALETTE;
}

EWXWEXPORT(int,expEVT_PALETTE_CHANGED)()
{
        return (int)wxEVT_PALETTE_CHANGED;
}

EWXWEXPORT(int,expEVT_JOY_BUTTON_DOWN)()
{
        return (int)wxEVT_JOY_BUTTON_DOWN;
}

EWXWEXPORT(int,expEVT_JOY_BUTTON_UP)()
{
        return (int)wxEVT_JOY_BUTTON_UP;
}

EWXWEXPORT(int,expEVT_JOY_MOVE)()
{
        return (int)wxEVT_JOY_MOVE;
}

EWXWEXPORT(int,expEVT_JOY_ZMOVE)()
{
        return (int)wxEVT_JOY_ZMOVE;
}

EWXWEXPORT(int,expEVT_DROP_FILES)()
{
        return (int)wxEVT_DROP_FILES;
}

EWXWEXPORT(int,expEVT_DRAW_ITEM)()
{
        return (int)wxEVT_DRAW_ITEM;
}

EWXWEXPORT(int,expEVT_MEASURE_ITEM)()
{
        return (int)wxEVT_MEASURE_ITEM;
}

EWXWEXPORT(int,expEVT_COMPARE_ITEM)()
{
        return (int)wxEVT_COMPARE_ITEM;
}

EWXWEXPORT(int,expEVT_INIT_DIALOG)()
{
        return (int)wxEVT_INIT_DIALOG;
}

EWXWEXPORT(int,expEVT_IDLE)()
{
        return (int)wxEVT_IDLE;
}

EWXWEXPORT(int,expEVT_UPDATE_UI)()
{
        return (int)wxEVT_UPDATE_UI;
}

EWXWEXPORT(int,expEVT_END_PROCESS)()
{
        return (int)wxEVT_END_PROCESS;
}

EWXWEXPORT(int,expEVT_DIALUP_CONNECTED)()
{
#if defined(__WXMAC__)
    return -1;
#else
        return (int)wxEVT_DIALUP_CONNECTED;
#endif
}

EWXWEXPORT(int,expEVT_DIALUP_DISCONNECTED)()
{
#if defined(__WXMAC__)
    return -1;
#else
        return (int)wxEVT_DIALUP_DISCONNECTED;
#endif
}

EWXWEXPORT(int,expEVT_COMMAND_LEFT_CLICK)()
{
        return (int)wxEVT_COMMAND_LEFT_CLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_LEFT_DCLICK)()
{
        return (int)wxEVT_COMMAND_LEFT_DCLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_RIGHT_CLICK)()
{
        return (int)wxEVT_COMMAND_RIGHT_CLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_RIGHT_DCLICK)()
{
        return (int)wxEVT_COMMAND_RIGHT_DCLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_SET_FOCUS)()
{
        return (int)wxEVT_COMMAND_SET_FOCUS;
}

EWXWEXPORT(int,expEVT_COMMAND_KILL_FOCUS)()
{
        return (int)wxEVT_COMMAND_KILL_FOCUS;
}

EWXWEXPORT(int,expEVT_COMMAND_ENTER)()
{
        return (int)wxEVT_COMMAND_ENTER;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_BEGIN_DRAG)()
{
        return (int)wxEVT_COMMAND_TREE_BEGIN_DRAG;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_BEGIN_RDRAG)()
{
        return (int)wxEVT_COMMAND_TREE_BEGIN_RDRAG;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_BEGIN_LABEL_EDIT)()
{
        return (int)wxEVT_COMMAND_TREE_BEGIN_LABEL_EDIT;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_END_LABEL_EDIT)()
{
        return (int)wxEVT_COMMAND_TREE_END_LABEL_EDIT;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_DELETE_ITEM)()
{
        return (int)wxEVT_COMMAND_TREE_DELETE_ITEM;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_GET_INFO)()
{
        return (int)wxEVT_COMMAND_TREE_GET_INFO;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_SET_INFO)()
{
        return (int)wxEVT_COMMAND_TREE_SET_INFO;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_ITEM_EXPANDED)()
{
        return (int)wxEVT_COMMAND_TREE_ITEM_EXPANDED;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_ITEM_EXPANDING)()
{
        return (int)wxEVT_COMMAND_TREE_ITEM_EXPANDING;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_ITEM_COLLAPSED)()
{
        return (int)wxEVT_COMMAND_TREE_ITEM_COLLAPSED;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_ITEM_COLLAPSING)()
{
        return (int)wxEVT_COMMAND_TREE_ITEM_COLLAPSING;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_SEL_CHANGED)()
{
        return (int)wxEVT_COMMAND_TREE_SEL_CHANGED;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_SEL_CHANGING)()
{
        return (int)wxEVT_COMMAND_TREE_SEL_CHANGING;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_KEY_DOWN)()
{
        return (int)wxEVT_COMMAND_TREE_KEY_DOWN;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_ITEM_ACTIVATED)()
{
        return (int)wxEVT_COMMAND_TREE_ITEM_ACTIVATED;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_ITEM_RIGHT_CLICK)()
{
        return (int)wxEVT_COMMAND_TREE_ITEM_RIGHT_CLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_ITEM_MIDDLE_CLICK)()
{
        return (int)wxEVT_COMMAND_TREE_ITEM_MIDDLE_CLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_TREE_END_DRAG)()
{
        return (int)wxEVT_COMMAND_TREE_END_DRAG;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_BEGIN_DRAG)()
{
        return (int)wxEVT_COMMAND_LIST_BEGIN_DRAG;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_BEGIN_RDRAG)()
{
        return (int)wxEVT_COMMAND_LIST_BEGIN_RDRAG;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_BEGIN_LABEL_EDIT)()
{
        return (int)wxEVT_COMMAND_LIST_BEGIN_LABEL_EDIT;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_END_LABEL_EDIT)()
{
        return (int)wxEVT_COMMAND_LIST_END_LABEL_EDIT;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_DELETE_ITEM)()
{
        return (int)wxEVT_COMMAND_LIST_DELETE_ITEM;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_DELETE_ALL_ITEMS)()
{
        return (int)wxEVT_COMMAND_LIST_DELETE_ALL_ITEMS;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_ITEM_SELECTED)()
{
        return (int)wxEVT_COMMAND_LIST_ITEM_SELECTED;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_ITEM_DESELECTED)()
{
        return (int)wxEVT_COMMAND_LIST_ITEM_DESELECTED;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_KEY_DOWN)()
{
        return (int)wxEVT_COMMAND_LIST_KEY_DOWN;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_INSERT_ITEM)()
{
        return (int)wxEVT_COMMAND_LIST_INSERT_ITEM;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_COL_CLICK)()
{
        return (int)wxEVT_COMMAND_LIST_COL_CLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_ITEM_RIGHT_CLICK)()
{
        return (int)wxEVT_COMMAND_LIST_ITEM_RIGHT_CLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_ITEM_MIDDLE_CLICK)()
{
        return (int)wxEVT_COMMAND_LIST_ITEM_MIDDLE_CLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_ITEM_ACTIVATED)()
{
        return (int)wxEVT_COMMAND_LIST_ITEM_ACTIVATED;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_ITEM_FOCUSED)()
{
        return (int)wxEVT_COMMAND_LIST_ITEM_FOCUSED;
}

EWXWEXPORT(int,expEVT_COMMAND_TAB_SEL_CHANGED)()
{
#if ((wxVERSION_NUMBER > 2800) && !defined(wxUSE_TAB_DIALOG)) || defined(__WXGTK__) || defined(__WXMAC__)
        return -1;
#else
        return (int)wxEVT_COMMAND_TAB_SEL_CHANGED;
#endif
}

EWXWEXPORT(int,expEVT_COMMAND_TAB_SEL_CHANGING)()
{
#if ((wxVERSION_NUMBER > 2800) && !defined(wxUSE_TAB_DIALOG)) || defined(__WXGTK__) || defined(__WXMAC__)
        return -1;
#else
        return (int)wxEVT_COMMAND_TAB_SEL_CHANGING;
#endif
}

EWXWEXPORT(int,expEVT_COMMAND_NOTEBOOK_PAGE_CHANGED)()
{
        return (int)wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED;
}

EWXWEXPORT(int,expEVT_COMMAND_NOTEBOOK_PAGE_CHANGING)()
{
        return (int)wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGING;
}

EWXWEXPORT(int,expEVT_COMMAND_SPLITTER_SASH_POS_CHANGED)()
{
        return (int)wxEVT_COMMAND_SPLITTER_SASH_POS_CHANGED;
}

EWXWEXPORT(int,expEVT_COMMAND_SPLITTER_SASH_POS_CHANGING)()
{
        return (int)wxEVT_COMMAND_SPLITTER_SASH_POS_CHANGING;
}

EWXWEXPORT(int,expEVT_COMMAND_SPLITTER_DOUBLECLICKED)()
{
        return (int)wxEVT_COMMAND_SPLITTER_DOUBLECLICKED;
}

EWXWEXPORT(int,expEVT_COMMAND_SPLITTER_UNSPLIT)()
{
        return (int)wxEVT_COMMAND_SPLITTER_UNSPLIT;
}

EWXWEXPORT(int,expEVT_WIZARD_PAGE_CHANGED)()
{
        return (int)wxEVT_WIZARD_PAGE_CHANGED;
}

EWXWEXPORT(int,expEVT_WIZARD_PAGE_CHANGING)()
{
        return (int)wxEVT_WIZARD_PAGE_CHANGING;
}

EWXWEXPORT(int,expEVT_WIZARD_CANCEL)()
{
        return (int)wxEVT_WIZARD_CANCEL;
}

EWXWEXPORT(int,expEVT_CALENDAR_SEL_CHANGED)()
{
        return (int)wxEVT_CALENDAR_SEL_CHANGED;
}

EWXWEXPORT(int,expEVT_CALENDAR_DAY_CHANGED)()
{
        return (int)wxEVT_CALENDAR_DAY_CHANGED;
}

EWXWEXPORT(int,expEVT_CALENDAR_MONTH_CHANGED)()
{
        return (int)wxEVT_CALENDAR_MONTH_CHANGED;
}

EWXWEXPORT(int,expEVT_CALENDAR_YEAR_CHANGED)()
{
        return (int)wxEVT_CALENDAR_YEAR_CHANGED;
}

EWXWEXPORT(int,expEVT_CALENDAR_DOUBLECLICKED)()
{
        return (int)wxEVT_CALENDAR_DOUBLECLICKED;
}

EWXWEXPORT(int,expEVT_CALENDAR_WEEKDAY_CLICKED)()
{
        return (int)wxEVT_CALENDAR_WEEKDAY_CLICKED;
}


#ifdef USE_CONTRIB
EWXWEXPORT(int,expEVT_PLOT_SEL_CHANGING)()
{
        return (int)wxEVT_PLOT_SEL_CHANGING;
}

EWXWEXPORT(int,expEVT_PLOT_SEL_CHANGED)()
{
        return (int)wxEVT_PLOT_SEL_CHANGED;
}

EWXWEXPORT(int,expEVT_PLOT_CLICKED)()
{
        return (int)wxEVT_PLOT_CLICKED;
}

EWXWEXPORT(int,expEVT_PLOT_DOUBLECLICKED)()
{
        return (int)wxEVT_PLOT_DOUBLECLICKED;
}

EWXWEXPORT(int,expEVT_PLOT_ZOOM_IN)()
{
        return (int)wxEVT_PLOT_ZOOM_IN;
}

EWXWEXPORT(int,expEVT_PLOT_ZOOM_OUT)()
{
        return (int)wxEVT_PLOT_ZOOM_OUT;
}

EWXWEXPORT(int,expEVT_PLOT_VALUE_SEL_CREATING)()
{
        return (int)wxEVT_PLOT_VALUE_SEL_CREATING;
}

EWXWEXPORT(int,expEVT_PLOT_VALUE_SEL_CREATED)()
{
        return (int)wxEVT_PLOT_VALUE_SEL_CREATED;
}

EWXWEXPORT(int,expEVT_PLOT_VALUE_SEL_CHANGING)()
{
        return (int)wxEVT_PLOT_VALUE_SEL_CHANGING;
}

EWXWEXPORT(int,expEVT_PLOT_VALUE_SEL_CHANGED)()
{
        return (int)wxEVT_PLOT_VALUE_SEL_CHANGED;
}

EWXWEXPORT(int,expEVT_PLOT_AREA_SEL_CREATING)()
{
        return (int)wxEVT_PLOT_AREA_SEL_CREATING;
}

EWXWEXPORT(int,expEVT_PLOT_AREA_SEL_CREATED)()
{
        return (int)wxEVT_PLOT_AREA_SEL_CREATED;
}

EWXWEXPORT(int,expEVT_PLOT_AREA_SEL_CHANGING)()
{
        return (int)wxEVT_PLOT_AREA_SEL_CHANGING;
}

EWXWEXPORT(int,expEVT_PLOT_AREA_SEL_CHANGED)()
{
        return (int)wxEVT_PLOT_AREA_SEL_CHANGED;
}

EWXWEXPORT(int,expEVT_PLOT_BEGIN_X_LABEL_EDIT)()
{
        return (int)wxEVT_PLOT_BEGIN_X_LABEL_EDIT;
}

EWXWEXPORT(int,expEVT_PLOT_END_X_LABEL_EDIT)()
{
        return (int)wxEVT_PLOT_END_X_LABEL_EDIT;
}

EWXWEXPORT(int,expEVT_PLOT_BEGIN_Y_LABEL_EDIT)()
{
        return (int)wxEVT_PLOT_BEGIN_Y_LABEL_EDIT;
}

EWXWEXPORT(int,expEVT_PLOT_END_Y_LABEL_EDIT)()
{
        return (int)wxEVT_PLOT_END_Y_LABEL_EDIT;
}

EWXWEXPORT(int,expEVT_PLOT_BEGIN_TITLE_EDIT)()
{
        return (int)wxEVT_PLOT_BEGIN_TITLE_EDIT;
}

EWXWEXPORT(int,expEVT_PLOT_END_TITLE_EDIT)()
{
        return (int)wxEVT_PLOT_END_TITLE_EDIT;
}

EWXWEXPORT(int,expEVT_PLOT_AREA_CREATE)()
{
        return (int)wxEVT_PLOT_AREA_CREATE;
}

EWXWEXPORT(int,expEVT_USER_FIRST)()
{
        return (int)wxEVT_USER_FIRST;
}

EWXWEXPORT(int,expEVT_DYNAMIC_SASH_SPLIT)()
{
        return (int)wxEVT_DYNAMIC_SASH_SPLIT;
}

EWXWEXPORT(int,expEVT_DYNAMIC_SASH_UNIFY)()
{
        return (int)wxEVT_DYNAMIC_SASH_UNIFY;
}
#endif /* USE_CONTRIB */

EWXWEXPORT(int,expEVT_HELP)()
{
        return (int)wxEVT_HELP;
}

EWXWEXPORT(int,expEVT_DETAILED_HELP)()
{
        return (int)wxEVT_DETAILED_HELP;
}


EWXWEXPORT(int,expEVT_GRID_CELL_LEFT_CLICK)()
{
        return (int)wxEVT_GRID_CELL_LEFT_CLICK;
}

EWXWEXPORT(int,expEVT_GRID_CELL_RIGHT_CLICK)()
{
        return (int)wxEVT_GRID_CELL_RIGHT_CLICK;
}

EWXWEXPORT(int,expEVT_GRID_CELL_LEFT_DCLICK)()
{
        return (int)wxEVT_GRID_CELL_LEFT_DCLICK;
}

EWXWEXPORT(int,expEVT_GRID_CELL_RIGHT_DCLICK)()
{
        return (int)wxEVT_GRID_CELL_RIGHT_DCLICK;
}

EWXWEXPORT(int,expEVT_GRID_LABEL_LEFT_CLICK)()
{
        return (int)wxEVT_GRID_LABEL_LEFT_CLICK;
}

EWXWEXPORT(int,expEVT_GRID_LABEL_RIGHT_CLICK)()
{
        return (int)wxEVT_GRID_LABEL_RIGHT_CLICK;
}

EWXWEXPORT(int,expEVT_GRID_LABEL_LEFT_DCLICK)()
{
        return (int)wxEVT_GRID_LABEL_LEFT_DCLICK;
}

EWXWEXPORT(int,expEVT_GRID_LABEL_RIGHT_DCLICK)()
{
        return (int)wxEVT_GRID_LABEL_RIGHT_DCLICK;
}

EWXWEXPORT(int,expEVT_GRID_ROW_SIZE)()
{
        return (int)wxEVT_GRID_ROW_SIZE;
}

EWXWEXPORT(int,expEVT_GRID_COL_SIZE)()
{
        return (int)wxEVT_GRID_COL_SIZE;
}

EWXWEXPORT(int,expEVT_GRID_RANGE_SELECT)()
{
        return (int)wxEVT_GRID_RANGE_SELECT;
}

EWXWEXPORT(int,expEVT_GRID_CELL_CHANGE)()
{
        return (int)wxEVT_GRID_CELL_CHANGE;
}

EWXWEXPORT(int,expEVT_GRID_SELECT_CELL)()
{
        return (int)wxEVT_GRID_SELECT_CELL;
}

EWXWEXPORT(int,expEVT_GRID_EDITOR_SHOWN)()
{
        return (int)wxEVT_GRID_EDITOR_SHOWN;
}

EWXWEXPORT(int,expEVT_GRID_EDITOR_HIDDEN)()
{
        return (int)wxEVT_GRID_EDITOR_HIDDEN;
}

EWXWEXPORT(int,expEVT_GRID_EDITOR_CREATED)()
{
        return (int)wxEVT_GRID_EDITOR_CREATED;
}

EWXWEXPORT(int,expK_BACK)()
{
        return (int)WXK_BACK;
}

EWXWEXPORT(int,expK_TAB)()
{
        return (int)WXK_TAB;
}

EWXWEXPORT(int,expK_RETURN)()
{
        return (int)WXK_RETURN;
}

EWXWEXPORT(int,expK_ESCAPE)()
{
        return (int)WXK_ESCAPE;
}

EWXWEXPORT(int,expK_SPACE)()
{
        return (int)WXK_SPACE;
}

EWXWEXPORT(int,expK_DELETE)()
{
        return (int)WXK_DELETE;
}

EWXWEXPORT(int,expK_START)()
{
        return (int)WXK_START;
}

EWXWEXPORT(int,expK_LBUTTON)()
{
        return (int)WXK_LBUTTON;
}

EWXWEXPORT(int,expK_RBUTTON)()
{
        return (int)WXK_RBUTTON;
}

EWXWEXPORT(int,expK_CANCEL)()
{
        return (int)WXK_CANCEL;
}

EWXWEXPORT(int,expK_MBUTTON)()
{
        return (int)WXK_MBUTTON;
}

EWXWEXPORT(int,expK_CLEAR)()
{
        return (int)WXK_CLEAR;
}

EWXWEXPORT(int,expK_SHIFT)()
{
        return (int)WXK_SHIFT;
}

EWXWEXPORT(int,expK_ALT)()
{
        return (int)WXK_ALT;
}

EWXWEXPORT(int,expK_CONTROL)()
{
        return (int)WXK_CONTROL;
}

EWXWEXPORT(int,expK_MENU)()
{
        return (int)WXK_MENU;
}

EWXWEXPORT(int,expK_PAUSE)()
{
        return (int)WXK_PAUSE;
}

EWXWEXPORT(int,expK_CAPITAL)()
{
        return (int)WXK_CAPITAL;
}

EWXWEXPORT(int,expK_END)()
{
        return (int)WXK_END;
}

EWXWEXPORT(int,expK_HOME)()
{
        return (int)WXK_HOME;
}

EWXWEXPORT(int,expK_LEFT)()
{
        return (int)WXK_LEFT;
}

EWXWEXPORT(int,expK_UP)()
{
        return (int)WXK_UP;
}

EWXWEXPORT(int,expK_RIGHT)()
{
        return (int)WXK_RIGHT;
}

EWXWEXPORT(int,expK_DOWN)()
{
        return (int)WXK_DOWN;
}

EWXWEXPORT(int,expK_SELECT)()
{
        return (int)WXK_SELECT;
}

EWXWEXPORT(int,expK_PRINT)()
{
        return (int)WXK_PRINT;
}

EWXWEXPORT(int,expK_EXECUTE)()
{
        return (int)WXK_EXECUTE;
}

EWXWEXPORT(int,expK_SNAPSHOT)()
{
        return (int)WXK_SNAPSHOT;
}

EWXWEXPORT(int,expK_INSERT)()
{
        return (int)WXK_INSERT;
}

EWXWEXPORT(int,expK_HELP)()
{
        return (int)WXK_HELP;
}

EWXWEXPORT(int,expK_NUMPAD0)()
{
        return (int)WXK_NUMPAD0;
}

EWXWEXPORT(int,expK_NUMPAD1)()
{
        return (int)WXK_NUMPAD1;
}

EWXWEXPORT(int,expK_NUMPAD2)()
{
        return (int)WXK_NUMPAD2;
}

EWXWEXPORT(int,expK_NUMPAD3)()
{
        return (int)WXK_NUMPAD3;
}

EWXWEXPORT(int,expK_NUMPAD4)()
{
        return (int)WXK_NUMPAD4;
}

EWXWEXPORT(int,expK_NUMPAD5)()
{
        return (int)WXK_NUMPAD5;
}

EWXWEXPORT(int,expK_NUMPAD6)()
{
        return (int)WXK_NUMPAD6;
}

EWXWEXPORT(int,expK_NUMPAD7)()
{
        return (int)WXK_NUMPAD7;
}

EWXWEXPORT(int,expK_NUMPAD8)()
{
        return (int)WXK_NUMPAD8;
}

EWXWEXPORT(int,expK_NUMPAD9)()
{
        return (int)WXK_NUMPAD9;
}

EWXWEXPORT(int,expK_MULTIPLY)()
{
        return (int)WXK_MULTIPLY;
}

EWXWEXPORT(int,expK_ADD)()
{
        return (int)WXK_ADD;
}

EWXWEXPORT(int,expK_SEPARATOR)()
{
        return (int)WXK_SEPARATOR;
}

EWXWEXPORT(int,expK_SUBTRACT)()
{
        return (int)WXK_SUBTRACT;
}

EWXWEXPORT(int,expK_DECIMAL)()
{
        return (int)WXK_DECIMAL;
}

EWXWEXPORT(int,expK_DIVIDE)()
{
        return (int)WXK_DIVIDE;
}

EWXWEXPORT(int,expK_F1)()
{
        return (int)WXK_F1;
}

EWXWEXPORT(int,expK_F2)()
{
        return (int)WXK_F2;
}

EWXWEXPORT(int,expK_F3)()
{
        return (int)WXK_F3;
}

EWXWEXPORT(int,expK_F4)()
{
        return (int)WXK_F4;
}

EWXWEXPORT(int,expK_F5)()
{
        return (int)WXK_F5;
}

EWXWEXPORT(int,expK_F6)()
{
        return (int)WXK_F6;
}

EWXWEXPORT(int,expK_F7)()
{
        return (int)WXK_F7;
}

EWXWEXPORT(int,expK_F8)()
{
        return (int)WXK_F8;
}

EWXWEXPORT(int,expK_F9)()
{
        return (int)WXK_F9;
}

EWXWEXPORT(int,expK_F10)()
{
        return (int)WXK_F10;
}

EWXWEXPORT(int,expK_F11)()
{
        return (int)WXK_F11;
}

EWXWEXPORT(int,expK_F12)()
{
        return (int)WXK_F12;
}

EWXWEXPORT(int,expK_F13)()
{
        return (int)WXK_F13;
}

EWXWEXPORT(int,expK_F14)()
{
        return (int)WXK_F14;
}

EWXWEXPORT(int,expK_F15)()
{
        return (int)WXK_F15;
}

EWXWEXPORT(int,expK_F16)()
{
        return (int)WXK_F16;
}

EWXWEXPORT(int,expK_F17)()
{
        return (int)WXK_F17;
}

EWXWEXPORT(int,expK_F18)()
{
        return (int)WXK_F18;
}

EWXWEXPORT(int,expK_F19)()
{
        return (int)WXK_F19;
}

EWXWEXPORT(int,expK_F20)()
{
        return (int)WXK_F20;
}

EWXWEXPORT(int,expK_F21)()
{
        return (int)WXK_F21;
}

EWXWEXPORT(int,expK_F22)()
{
        return (int)WXK_F22;
}

EWXWEXPORT(int,expK_F23)()
{
        return (int)WXK_F23;
}

EWXWEXPORT(int,expK_F24)()
{
        return (int)WXK_F24;
}

EWXWEXPORT(int,expK_NUMLOCK)()
{
        return (int)WXK_NUMLOCK;
}

EWXWEXPORT(int,expK_SCROLL)()
{
        return (int)WXK_SCROLL;
}

EWXWEXPORT(int,expK_PAGEUP)()
{
        return (int)WXK_PAGEUP;
}

EWXWEXPORT(int,expK_PAGEDOWN)()
{
        return (int)WXK_PAGEDOWN;
}

EWXWEXPORT(int,expK_NUMPAD_SPACE)()
{
        return (int)WXK_NUMPAD_SPACE;
}

EWXWEXPORT(int,expK_NUMPAD_TAB)()
{
        return (int)WXK_NUMPAD_TAB;
}

EWXWEXPORT(int,expK_NUMPAD_ENTER)()
{
        return (int)WXK_NUMPAD_ENTER;
}

EWXWEXPORT(int,expK_NUMPAD_F1)()
{
        return (int)WXK_NUMPAD_F1;
}

EWXWEXPORT(int,expK_NUMPAD_F2)()
{
        return (int)WXK_NUMPAD_F2;
}

EWXWEXPORT(int,expK_NUMPAD_F3)()
{
        return (int)WXK_NUMPAD_F3;
}

EWXWEXPORT(int,expK_NUMPAD_F4)()
{
        return (int)WXK_NUMPAD_F4;
}

EWXWEXPORT(int,expK_NUMPAD_HOME)()
{
        return (int)WXK_NUMPAD_HOME;
}

EWXWEXPORT(int,expK_NUMPAD_LEFT)()
{
        return (int)WXK_NUMPAD_LEFT;
}

EWXWEXPORT(int,expK_NUMPAD_UP)()
{
        return (int)WXK_NUMPAD_UP;
}

EWXWEXPORT(int,expK_NUMPAD_RIGHT)()
{
        return (int)WXK_NUMPAD_RIGHT;
}

EWXWEXPORT(int,expK_NUMPAD_DOWN)()
{
        return (int)WXK_NUMPAD_DOWN;
}

EWXWEXPORT(int,expK_NUMPAD_PAGEUP)()
{
        return (int)WXK_NUMPAD_PAGEUP;
}

EWXWEXPORT(int,expK_NUMPAD_PAGEDOWN)()
{
        return (int)WXK_NUMPAD_PAGEDOWN;
}

EWXWEXPORT(int,expK_NUMPAD_END)()
{
        return (int)WXK_NUMPAD_END;
}

EWXWEXPORT(int,expK_NUMPAD_BEGIN)()
{
        return (int)WXK_NUMPAD_BEGIN;
}

EWXWEXPORT(int,expK_NUMPAD_INSERT)()
{
        return (int)WXK_NUMPAD_INSERT;
}

EWXWEXPORT(int,expK_NUMPAD_DELETE)()
{
        return (int)WXK_NUMPAD_DELETE;
}

EWXWEXPORT(int,expK_NUMPAD_EQUAL)()
{
        return (int)WXK_NUMPAD_EQUAL;
}

EWXWEXPORT(int,expK_NUMPAD_MULTIPLY)()
{
        return (int)WXK_NUMPAD_MULTIPLY;
}

EWXWEXPORT(int,expK_NUMPAD_ADD)()
{
        return (int)WXK_NUMPAD_ADD;
}

EWXWEXPORT(int,expK_NUMPAD_SEPARATOR)()
{
        return (int)WXK_NUMPAD_SEPARATOR;
}

EWXWEXPORT(int,expK_NUMPAD_SUBTRACT)()
{
        return (int)WXK_NUMPAD_SUBTRACT;
}

EWXWEXPORT(int,expK_NUMPAD_DECIMAL)()
{
        return (int)WXK_NUMPAD_DECIMAL;
}

EWXWEXPORT(int,expK_NUMPAD_DIVIDE)()
{
        return (int)WXK_NUMPAD_DIVIDE;
}

EWXWEXPORT(int,expK_WINDOWS_LEFT)()
{
        return (int)WXK_WINDOWS_LEFT;
}

EWXWEXPORT(int,expK_WINDOWS_RIGHT)()
{
        return (int)WXK_WINDOWS_RIGHT;
}

EWXWEXPORT(int,expK_WINDOWS_MENU)()
{
        return (int)WXK_WINDOWS_MENU;
}

EWXWEXPORT(int,expK_COMMAND)()
{
        return (int)WXK_COMMAND;
}

EWXWEXPORT(int,expK_SPECIAL1)()
{
        return (int)WXK_SPECIAL1;
}

EWXWEXPORT(int,expK_SPECIAL2)()
{
        return (int)WXK_SPECIAL2;
}

EWXWEXPORT(int,expK_SPECIAL3)()
{
        return (int)WXK_SPECIAL3;
}

EWXWEXPORT(int,expK_SPECIAL4)()
{
        return (int)WXK_SPECIAL4;
}

EWXWEXPORT(int,expK_WXK_SPECIAL5)()
{
        return (int)WXK_SPECIAL5;
}

EWXWEXPORT(int,expK_SPECIAL6)()
{
        return (int)WXK_SPECIAL6;
}

EWXWEXPORT(int,expK_SPECIAL7)()
{
        return (int)WXK_SPECIAL7;
}

EWXWEXPORT(int,expK_SPECIAL8)()
{
        return (int)WXK_SPECIAL8;
}

EWXWEXPORT(int,expK_SPECIAL9)()
{
        return (int)WXK_SPECIAL9;
}

EWXWEXPORT(int,expK_SPECIAL10)()
{
        return (int)WXK_SPECIAL10;
}

EWXWEXPORT(int,expK_SPECIAL11)()
{
        return (int)WXK_SPECIAL11;
}

EWXWEXPORT(int,expK_SPECIAL12)()
{
        return (int)WXK_SPECIAL12;
}

EWXWEXPORT(int,expK_SPECIAL13)()
{
        return (int)WXK_SPECIAL13;
}

EWXWEXPORT(int,expK_SPECIAL14)()
{
        return (int)WXK_SPECIAL14;
}

EWXWEXPORT(int,expK_SPECIAL15)()
{
        return (int)WXK_SPECIAL15;
}

EWXWEXPORT(int,expK_SPECIAL16)()
{
        return (int)WXK_SPECIAL16;
}

EWXWEXPORT(int,expK_SPECIAL17)()
{
        return (int)WXK_SPECIAL17;
}

EWXWEXPORT(int,expK_SPECIAL18)()
{
        return (int)WXK_SPECIAL18;
}

EWXWEXPORT(int,expK_SPECIAL19)()
{
        return (int)WXK_SPECIAL19;
}

EWXWEXPORT(int,expK_SPECIAL20)()
{
        return (int)WXK_SPECIAL20;
}

}
