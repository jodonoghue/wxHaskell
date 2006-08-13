#include "wrapper.h"
#include "wx/process.h"
#include "wx/dialup.h"
#include "wx/tabctrl.h"
#include "wx/plot/plot.h"
#include "wx/gizmos/dynamicsash.h"

extern "C"
{

EWXWEXPORT(void*,wxCommandEvent_Create)(int _typ, int _id)
{
	return new wxCommandEvent((wxEventType)_typ, _id);
}

EWXWEXPORT(void,wxCommandEvent_Delete)(void* _obj)
{
	delete (wxCommandEvent*)_obj;
}

EWXWEXPORT(void, wxEvent_Skip)(void* _obj)
{
	((wxEvent*)_obj)->Skip();
}

EWXWEXPORT(int, wxEvent_GetEventType)(void* _obj)
{
	return (int)((wxEvent*)_obj)->GetEventType();
}
	
EWXWEXPORT(void, wxEvent_SetEventType)(void* _obj, int typ)
{
	((wxEvent*)_obj)->SetEventType((wxEventType) typ);
}
	
EWXWEXPORT(void*, wxEvent_GetEventObject)(void* _obj)
{
	return (void*)((wxEvent*)_obj)->GetEventObject();
}
	
EWXWEXPORT(void, wxEvent_SetEventObject)(void* _obj, void* obj)
{
	((wxEvent*)_obj)->SetEventObject((wxObject*) obj);
}
	
EWXWEXPORT(int, wxEvent_GetTimestamp)(void* _obj)
{
	return (int)((wxEvent*)_obj)->GetTimestamp();
}
	
EWXWEXPORT(void, wxEvent_SetTimestamp)(void* _obj, int ts)
{
	((wxEvent*)_obj)->SetTimestamp((long)ts);
}
	
EWXWEXPORT(int, wxEvent_GetId)(void* _obj)
{
	return ((wxEvent*)_obj)->GetId();
}
	
EWXWEXPORT(void, wxEvent_SetId)(void* _obj, int Id)
{
	((wxEvent*)_obj)->SetId(Id);
}
	
EWXWEXPORT(int, wxEvent_GetSkipped)(void* _obj)
{
	return (int)((wxEvent*)_obj)->GetSkipped();
}
	
EWXWEXPORT(int, wxEvent_IsCommandEvent)(void* _obj)
{
	return (int)((wxEvent*)_obj)->IsCommandEvent();
}
	
EWXWEXPORT(void, wxEvent_CopyObject)(void* _obj, void* object_dest)
{
#if wxVERSION_NUMBER < 2400
	((wxEvent*)_obj)->CopyObject(*((wxObject*) object_dest));
#endif
}
	
EWXWEXPORT(void, wxEvent_SetCallbackUserData)(void* _obj, wxObject* obj)
{
	((wxEvent*)_obj)->m_callbackUserData = (wxObject*) obj;
}
	
EWXWEXPORT(void*, wxEvent_GetCallbackUserData)(void* _obj)
{
	return (void*)((wxEvent*)_obj)->m_callbackUserData;
}
	
EWXWEXPORT(void, wxCommandEvent_SetClientData)(void* _obj, void* clientData)
{
	((wxCommandEvent*)_obj)->SetClientData(clientData);
}
	
EWXWEXPORT(void*, wxCommandEvent_GetClientData)(void* _obj)
{
	return ((wxCommandEvent*)_obj)->m_clientData;
}
	
EWXWEXPORT(void, wxCommandEvent_SetClientObject)(void* _obj, void* clientObject)
{
	((wxCommandEvent*)_obj)->SetClientObject((wxClientData*) clientObject);
}
	
EWXWEXPORT(void*, wxCommandEvent_GetClientObject)(void* _obj)
{
	return (void*)((wxCommandEvent*)_obj)->GetClientObject();
}
	
EWXWEXPORT(int, wxCommandEvent_GetSelection)(void* _obj)
{
	return ((wxCommandEvent*)_obj)->GetSelection();
}
	
EWXWEXPORT(void, wxCommandEvent_SetString)(void* _obj, char* s)
{
	((wxCommandEvent*)_obj)->SetString(s);
}
	
EWXWEXPORT(int, wxCommandEvent_GetString)(void* _obj, void* _buf)
{
	wxString result = ((wxCommandEvent*)_obj)->GetString();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxCommandEvent_IsChecked)(void* _obj)
{
	return (int)((wxCommandEvent*)_obj)->IsChecked();
}
	
EWXWEXPORT(int, wxCommandEvent_IsSelection)(void* _obj)
{
	return (int)((wxCommandEvent*)_obj)->IsSelection();
}
	
EWXWEXPORT(void, wxCommandEvent_SetExtraLong)(void* _obj, long extraLong)
{
	((wxCommandEvent*)_obj)->SetExtraLong(extraLong);
}
	
EWXWEXPORT(long, wxCommandEvent_GetExtraLong)(void* _obj)
{
	return ((wxCommandEvent*)_obj)->m_extraLong;
}
	
EWXWEXPORT(void, wxCommandEvent_SetInt)(void* _obj, int i)
{
	((wxCommandEvent*)_obj)->SetInt(i);
}
	
EWXWEXPORT(long, wxCommandEvent_GetInt)(void* _obj)
{
	return ((wxCommandEvent*)_obj)->GetInt();
}
	
EWXWEXPORT(void, wxCommandEvent_CopyObject)(void* _obj, void* object_dest)
{
#if wxVERSION_NUMBER < 2400
	((wxCommandEvent*)_obj)->CopyObject(*((wxObject*) object_dest));
#endif
}
	
EWXWEXPORT(void, wxNotifyEvent_Veto)(void* _obj)
{
	((wxNotifyEvent*)_obj)->Veto();
}
	
EWXWEXPORT(void, wxNotifyEvent_Allow)(void* _obj)
{
	((wxNotifyEvent*)_obj)->Allow();
}
	
EWXWEXPORT(int, wxNotifyEvent_IsAllowed)(void* _obj)
{
	return (int)((wxNotifyEvent*)_obj)->IsAllowed();
}
	
EWXWEXPORT(void, wxNotifyEvent_CopyObject)(void* _obj, void* object_dest)
{
#if wxVERSION_NUMBER < 2400
	((wxNotifyEvent*)_obj)->CopyObject(*((wxObject*) object_dest));
#endif
}
	
EWXWEXPORT(int, wxScrollWinEvent_GetOrientation)(void* _obj)
{
	return ((wxScrollWinEvent*)_obj)->GetOrientation();
}
	
EWXWEXPORT(int, wxScrollWinEvent_GetPosition)(void* _obj)
{
	return ((wxScrollWinEvent*)_obj)->GetPosition();
}
	
EWXWEXPORT(void, wxScrollWinEvent_SetOrientation)(void* _obj, int orient)
{
	((wxScrollWinEvent*)_obj)->SetOrientation(orient);
}
	
EWXWEXPORT(void, wxScrollWinEvent_SetPosition)(void* _obj, int pos)
{
	((wxScrollWinEvent*)_obj)->SetPosition(pos);
}
	
EWXWEXPORT(int, wxMouseEvent_IsButton)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->IsButton();
}
	
EWXWEXPORT(int, wxMouseEvent_ButtonDown)(void* _obj, int but)
{
	return (int)((wxMouseEvent*)_obj)->ButtonDown(but);
}
	
EWXWEXPORT(int, wxMouseEvent_ButtonDClick)(void* _obj, int but)
{
	return (int)((wxMouseEvent*)_obj)->ButtonDClick(but);
}
	
EWXWEXPORT(int, wxMouseEvent_ButtonUp)(void* _obj, int but)
{
	return (int)((wxMouseEvent*)_obj)->ButtonUp(but);
}
	
EWXWEXPORT(int, wxMouseEvent_Button)(void* _obj, int but)
{
	return (int)((wxMouseEvent*)_obj)->Button(but);
}
	
EWXWEXPORT(int, wxMouseEvent_ButtonIsDown)(void* _obj, int but)
{
	return (int)((wxMouseEvent*)_obj)->ButtonIsDown(but);
}
	
EWXWEXPORT(int, wxMouseEvent_ControlDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->ControlDown();
}
	
EWXWEXPORT(int, wxMouseEvent_MetaDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->MetaDown();
}
	
EWXWEXPORT(int, wxMouseEvent_AltDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->AltDown();
}
	
EWXWEXPORT(int, wxMouseEvent_ShiftDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->ShiftDown();
}
	
EWXWEXPORT(int, wxMouseEvent_LeftDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->LeftDown();
}
	
EWXWEXPORT(int, wxMouseEvent_MiddleDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->MiddleDown();
}
	
EWXWEXPORT(int, wxMouseEvent_RightDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->RightDown();
}
	
EWXWEXPORT(int, wxMouseEvent_LeftUp)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->LeftUp();
}
	
EWXWEXPORT(int, wxMouseEvent_MiddleUp)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->MiddleUp();
}
	
EWXWEXPORT(int, wxMouseEvent_RightUp)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->RightUp();
}
	
EWXWEXPORT(int, wxMouseEvent_LeftDClick)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->LeftDClick();
}
	
EWXWEXPORT(int, wxMouseEvent_MiddleDClick)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->MiddleDClick();
}
	
EWXWEXPORT(int, wxMouseEvent_RightDClick)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->RightDClick();
}
	
EWXWEXPORT(int, wxMouseEvent_LeftIsDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->LeftIsDown();
}
	
EWXWEXPORT(int, wxMouseEvent_MiddleIsDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->MiddleIsDown();
}
	
EWXWEXPORT(int, wxMouseEvent_RightIsDown)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->RightIsDown();
}
	
EWXWEXPORT(int, wxMouseEvent_Dragging)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->Dragging();
}
	
EWXWEXPORT(int, wxMouseEvent_Moving)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->Moving();
}
	
EWXWEXPORT(int, wxMouseEvent_Entering)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->Entering();
}
	
EWXWEXPORT(int, wxMouseEvent_Leaving)(void* _obj)
{
	return (int)((wxMouseEvent*)_obj)->Leaving();
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
	
EWXWEXPORT(int, wxMouseEvent_GetX)(void* _obj)
{
	return ((wxMouseEvent*)_obj)->GetX();
}
	
EWXWEXPORT(int, wxMouseEvent_GetY)(void* _obj)
{
	return ((wxMouseEvent*)_obj)->GetY();
}
	
EWXWEXPORT(void, wxMouseEvent_CopyObject)(void* _obj, void* object_dest)
{
#if wxVERSION_NUMBER < 2400
	((wxMouseEvent*)_obj)->CopyObject(*((wxObject*) object_dest));
#endif
}
	
EWXWEXPORT(int, wxSetCursorEvent_GetX)(void* _obj)
{
	return (int)((wxSetCursorEvent*)_obj)->GetX();
}
	
EWXWEXPORT(int, wxSetCursorEvent_GetY)(void* _obj)
{
	return (int)((wxSetCursorEvent*)_obj)->GetY();
}
	
EWXWEXPORT(void, wxSetCursorEvent_SetCursor)(void* _obj, void* cursor)
{
	((wxSetCursorEvent*)_obj)->SetCursor(*((wxCursor*)cursor));
}
	
EWXWEXPORT(void*, wxSetCursorEvent_GetCursor)(void* _obj)
{
	return (void*)(&((wxSetCursorEvent*)_obj)->GetCursor());
}
	
EWXWEXPORT(int, wxSetCursorEvent_HasCursor)(void* _obj)
{
	return (int)((wxSetCursorEvent*)_obj)->HasCursor();
}
	
EWXWEXPORT(int, wxKeyEvent_ControlDown)(void* _obj)
{
	return (int)((wxKeyEvent*)_obj)->ControlDown();
}
	
EWXWEXPORT(int, wxKeyEvent_MetaDown)(void* _obj)
{
	return (int)((wxKeyEvent*)_obj)->MetaDown();
}
	
EWXWEXPORT(int, wxKeyEvent_AltDown)(void* _obj)
{
	return (int)((wxKeyEvent*)_obj)->AltDown();
}
	
EWXWEXPORT(int, wxKeyEvent_ShiftDown)(void* _obj)
{
	return (int)((wxKeyEvent*)_obj)->ShiftDown();
}
	
EWXWEXPORT(int, wxKeyEvent_HasModifiers)(void* _obj)
{
	return (int)((wxKeyEvent*)_obj)->HasModifiers();
}
	
EWXWEXPORT(int, wxKeyEvent_GetKeyCode)(void* _obj)
{
	return ((wxKeyEvent*)_obj)->GetKeyCode();
}
	
EWXWEXPORT(void, wxKeyEvent_SetKeyCode)(void* _obj, int code)
{
	((wxKeyEvent*)_obj)->m_keyCode = code;
}
	
EWXWEXPORT(void, wxKeyEvent_GetPosition)(void* _obj, int* xpos, int* ypos)
{
	((wxKeyEvent*)_obj)->GetPosition((wxCoord*) xpos, (wxCoord*) ypos);
}
	
EWXWEXPORT(int, wxKeyEvent_GetX)(void* _obj)
{
	return (int)((wxKeyEvent*)_obj)->GetX();
}
	
EWXWEXPORT(int, wxKeyEvent_GetY)(void* _obj)
{
	return (int)((wxKeyEvent*)_obj)->GetY();
}
	
EWXWEXPORT(void, wxKeyEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxKeyEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}
	
EWXWEXPORT(void, wxSizeEvent_GetSize)(void* _obj, int* w, int* h)
{
	wxSize sz = ((wxSizeEvent*)_obj)->GetSize();
	*w = sz.x;
	*h = sz.y;
}
	
EWXWEXPORT(void, wxSizeEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxSizeEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}
	
EWXWEXPORT(void, wxMoveEvent_GetPosition)(void* _obj, int* x, int* y)
{
	wxPoint pt = ((wxMoveEvent*)_obj)->GetPosition();
	*x = pt.x;
	*y = pt.y;
}
	
EWXWEXPORT(void, wxMoveEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxMoveEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}
	
EWXWEXPORT(void*, wxEraseEvent_GetDC)(void* _obj)
{
	return (void*)((wxEraseEvent*)_obj)->GetDC();
}
	
EWXWEXPORT(void, wxEraseEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxEraseEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}

EWXWEXPORT(int, wxActivateEvent_GetActive)(void* _obj)
{
	return (int)((wxActivateEvent*)_obj)->GetActive();
}
	
EWXWEXPORT(void, wxActivateEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxActivateEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}

EWXWEXPORT(int, wxMenuEvent_GetMenuId)(void* _obj)
{
	return ((wxMenuEvent*)_obj)->GetMenuId();
}
	
EWXWEXPORT(void, wxMenuEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxMenuEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}

EWXWEXPORT(void, wxCloseEvent_SetLoggingOff)(void* _obj, int logOff)
{
	((wxCloseEvent*)_obj)->SetLoggingOff(logOff != 0);
}
	
EWXWEXPORT(int, wxCloseEvent_GetLoggingOff)(void* _obj)
{
	return (int)((wxCloseEvent*)_obj)->GetLoggingOff();
}
	
EWXWEXPORT(void, wxCloseEvent_Veto)(void* _obj, int veto)
{
	((wxCloseEvent*)_obj)->Veto(veto != 0);
}
	
EWXWEXPORT(void, wxCloseEvent_SetCanVeto)(void* _obj, int canVeto)
{
	((wxCloseEvent*)_obj)->SetCanVeto(canVeto != 0);
}
	
EWXWEXPORT(int, wxCloseEvent_CanVeto)(void* _obj)
{
	return (int)((wxCloseEvent*)_obj)->CanVeto();
}
	
EWXWEXPORT(int, wxCloseEvent_GetVeto)(void* _obj)
{
	return (int)((wxCloseEvent*)_obj)->GetVeto();
}
	
EWXWEXPORT(void, wxCloseEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxCloseEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}

EWXWEXPORT(void, wxShowEvent_SetShow)(void* _obj, int show)
{
	((wxShowEvent*)_obj)->SetShow(show != 0);
}
	
EWXWEXPORT(int, wxShowEvent_GetShow)(void* _obj)
{
	return (int)((wxShowEvent*)_obj)->GetShow();
}
	
EWXWEXPORT(void, wxShowEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxShowEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}

EWXWEXPORT(void, wxJoystickEvent_GetPosition)(void* _obj, int* x, int* y)
{
	wxPoint pt = ((wxJoystickEvent*)_obj)->GetPosition();
	*x = pt.x;
	*y = pt.y;
}
	
EWXWEXPORT(int, wxJoystickEvent_GetZPosition)(void* _obj)
{
	return ((wxJoystickEvent*)_obj)->GetZPosition();
}
	
EWXWEXPORT(int, wxJoystickEvent_GetButtonState)(void* _obj)
{
	return ((wxJoystickEvent*)_obj)->GetButtonState();
}
	
EWXWEXPORT(int, wxJoystickEvent_GetButtonChange)(void* _obj)
{
	return ((wxJoystickEvent*)_obj)->GetButtonChange();
}
	
EWXWEXPORT(int, wxJoystickEvent_GetJoystick)(void* _obj)
{
	return ((wxJoystickEvent*)_obj)->GetJoystick();
}
	
EWXWEXPORT(void, wxJoystickEvent_SetJoystick)(void* _obj, int stick)
{
	((wxJoystickEvent*)_obj)->SetJoystick(stick);
}
	
EWXWEXPORT(void, wxJoystickEvent_SetButtonState)(void* _obj, int state)
{
	((wxJoystickEvent*)_obj)->SetButtonState(state);
}
	
EWXWEXPORT(void, wxJoystickEvent_SetButtonChange)(void* _obj, int change)
{
	((wxJoystickEvent*)_obj)->SetButtonChange(change);
}
	
EWXWEXPORT(void, wxJoystickEvent_SetPosition)(void* _obj, void* pos)
{
	((wxJoystickEvent*)_obj)->SetPosition(*((wxPoint*)pos));
}
	
EWXWEXPORT(void, wxJoystickEvent_SetZPosition)(void* _obj, int zPos)
{
	((wxJoystickEvent*)_obj)->SetZPosition(zPos);
}
	
EWXWEXPORT(int, wxJoystickEvent_IsButton)(void* _obj)
{
	return (int)((wxJoystickEvent*)_obj)->IsButton();
}
	
EWXWEXPORT(int, wxJoystickEvent_IsMove)(void* _obj)
{
	return (int)((wxJoystickEvent*)_obj)->IsMove();
}
	
EWXWEXPORT(int, wxJoystickEvent_IsZMove)(void* _obj)
{
	return (int)((wxJoystickEvent*)_obj)->IsZMove();
}
	
EWXWEXPORT(int, wxJoystickEvent_ButtonDown)(void* _obj, int but)
{
	return (int)((wxJoystickEvent*)_obj)->ButtonDown(but);
}
	
EWXWEXPORT(int, wxJoystickEvent_ButtonUp)(void* _obj, int but)
{
	return (int)((wxJoystickEvent*)_obj)->ButtonUp(but);
}
	
EWXWEXPORT(int, wxJoystickEvent_ButtonIsDown)(void* _obj, int but)
{
	return (int)((wxJoystickEvent*)_obj)->ButtonIsDown(but);
}
	
EWXWEXPORT(void, wxJoystickEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxJoystickEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}
	
EWXWEXPORT(int, wxUpdateUIEvent_GetChecked)(void* _obj)
{
	return (int)((wxUpdateUIEvent*)_obj)->GetChecked();
}
	
EWXWEXPORT(int, wxUpdateUIEvent_GetEnabled)(void* _obj)
{
	return (int)((wxUpdateUIEvent*)_obj)->GetEnabled();
}
	
EWXWEXPORT(int, wxUpdateUIEvent_GetText)(void* _obj, void* _buf)
{
	wxString result =((wxUpdateUIEvent*)_obj)->GetText();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxUpdateUIEvent_GetSetText)(void* _obj)
{
	return (int)((wxUpdateUIEvent*)_obj)->GetSetText();
}
	
EWXWEXPORT(int, wxUpdateUIEvent_GetSetChecked)(void* _obj)
{
	return (int)((wxUpdateUIEvent*)_obj)->GetSetChecked();
}
	
EWXWEXPORT(int, wxUpdateUIEvent_GetSetEnabled)(void* _obj)
{
	return (int)((wxUpdateUIEvent*)_obj)->GetSetEnabled();
}
	
EWXWEXPORT(void, wxUpdateUIEvent_Check)(void* _obj, int check)
{
	((wxUpdateUIEvent*)_obj)->Check(check != 0);
}
	
EWXWEXPORT(void, wxUpdateUIEvent_Enable)(void* _obj, int enable)
{
	((wxUpdateUIEvent*)_obj)->Enable(enable != 0);
}
	
EWXWEXPORT(void, wxUpdateUIEvent_SetText)(void* _obj, char* text)
{
	((wxUpdateUIEvent*)_obj)->SetText(text);
}
	
EWXWEXPORT(void, wxUpdateUIEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxUpdateUIEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}

EWXWEXPORT(void, wxPaletteChangedEvent_SetChangedWindow)(void* _obj, void* win)
{
	((wxPaletteChangedEvent*)_obj)->SetChangedWindow((wxWindow*) win);
}
	
EWXWEXPORT(void*, wxPaletteChangedEvent_GetChangedWindow)(void* _obj)
{
	return (void*)((wxPaletteChangedEvent*)_obj)->GetChangedWindow();
}
	
EWXWEXPORT(void, wxPaletteChangedEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxPaletteChangedEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}

EWXWEXPORT(void, wxQueryNewPaletteEvent_SetPaletteRealized)(void* _obj, int realized)
{
	((wxQueryNewPaletteEvent*)_obj)->SetPaletteRealized(realized != 0);
}
	
EWXWEXPORT(int, wxQueryNewPaletteEvent_GetPaletteRealized)(void* _obj)
{
	return (int)((wxQueryNewPaletteEvent*)_obj)->GetPaletteRealized();
}
	
EWXWEXPORT(void, wxQueryNewPaletteEvent_CopyObject)(void* _obj, void* obj)
{
#if wxVERSION_NUMBER < 2400
	((wxQueryNewPaletteEvent*)_obj)->CopyObject(*((wxObject*)obj));
#endif
}

EWXWEXPORT(int, wxNavigationKeyEvent_GetDirection)(void* _obj)
{
	return (int)((wxNavigationKeyEvent*)_obj)->GetDirection();
}
	
EWXWEXPORT(void, wxNavigationKeyEvent_SetDirection)(void* _obj, int bForward)
{
	((wxNavigationKeyEvent*)_obj)->SetDirection(bForward != 0);
}
	
EWXWEXPORT(int, wxNavigationKeyEvent_IsWindowChange)(void* _obj)
{
	return (int)((wxNavigationKeyEvent*)_obj)->IsWindowChange();
}
	
EWXWEXPORT(void, wxNavigationKeyEvent_SetWindowChange)(void* _obj, int bIs)
{
	((wxNavigationKeyEvent*)_obj)->SetWindowChange(bIs != 0);
}
	
EWXWEXPORT(int, wxNavigationKeyEvent_ShouldPropagate)(void* _obj)
{
	return (int)((wxNavigationKeyEvent*)_obj)->ShouldPropagate();
}
	
EWXWEXPORT(void, wxNavigationKeyEvent_SetPropagate)(void* _obj, int bDoIt)
{
	((wxNavigationKeyEvent*)_obj)->SetPropagate(bDoIt != 0);
}
	
EWXWEXPORT(void*, wxNavigationKeyEvent_GetCurrentFocus)(void* _obj)
{
	return (void*)((wxNavigationKeyEvent*)_obj)->GetCurrentFocus();
}
	
EWXWEXPORT(void, wxNavigationKeyEvent_SetCurrentFocus)(void* _obj, void* win)
{
	((wxNavigationKeyEvent*)_obj)->SetCurrentFocus((wxWindow*)win);
}
	
EWXWEXPORT(void*, wxWindowCreateEvent_GetWindow)(void* _obj)
{
	return (void*)((wxWindowCreateEvent*)_obj)->GetWindow();
}
	
EWXWEXPORT(void*, wxWindowDestroyEvent_GetWindow)(void* _obj)
{
	return (void*)((wxWindowDestroyEvent*)_obj)->GetWindow();
}
	
EWXWEXPORT(void, wxIdleEvent_RequestMore)(void* _obj, int needMore)
{
	((wxIdleEvent*)_obj)->RequestMore(needMore != 0);
}
	
EWXWEXPORT(int, wxIdleEvent_MoreRequested)(void* _obj)
{
	return (int)((wxIdleEvent*)_obj)->MoreRequested();
}
	
EWXWEXPORT(void, wxIdleEvent_CopyObject)(void* _obj, void* object_dest)
{
#if wxVERSION_NUMBER < 2400
	((wxIdleEvent*)_obj)->CopyObject(*((wxObject*) object_dest));
#endif
}
	
EWXWEXPORT(int, wxListEvent_GetCode)(void* _obj)
{
#if wxVERSION_NUMBER < 2400
	return ((wxListEvent*)_obj)->GetCode();
#else
	return ((wxListEvent*)_obj)->GetKeyCode();
#endif
}
	
EWXWEXPORT(int, wxListEvent_GetIndex)(void* _obj)
{
	return (int)((wxListEvent*)_obj)->GetIndex();
}
	
EWXWEXPORT(int, wxListEvent_GetOldIndex)(void* _obj)
{
#if wxVERSION_NUMBER < 2400
	return (int)((wxListEvent*)_obj)->GetOldIndex();
#else
	return 0;
#endif
}
	
EWXWEXPORT(int, wxListEvent_GetOldItem)(void* _obj)
{
#if wxVERSION_NUMBER < 2400
	return (int)((wxListEvent*)_obj)->GetOldItem();
#else
	return 0;
#endif
}
	
EWXWEXPORT(int, wxListEvent_GetColumn)(void* _obj)
{
	return ((wxListEvent*)_obj)->GetColumn();
}
	
EWXWEXPORT(int, wxListEvent_Cancelled)(void* _obj)
{
#if wxVERSION_NUMBER < 2400
	return (int)((wxListEvent*)_obj)->Cancelled();
#else
	return 0;
#endif
}
	
EWXWEXPORT(void, wxListEvent_GetPoint)(void* _obj, void* x, void* y)
{
	wxPoint pos = ((wxListEvent*)_obj)->GetPoint();
	*((int*)x) = pos.x;
	*((int*)y) = pos.y;
}
	
EWXWEXPORT(int, wxListEvent_GetLabel)(void* _obj, void* _buf)
{
	wxString result = ((wxListEvent*)_obj)->GetLabel();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxListEvent_GetText)(void* _obj, void* _buf)
{
	wxString result = ((wxListEvent*)_obj)->GetText();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxListEvent_GetImage)(void* _obj)
{
	return ((wxListEvent*)_obj)->GetImage();
}
	
EWXWEXPORT(int, wxListEvent_GetData)(void* _obj)
{
	return (int)((wxListEvent*)_obj)->GetData();
}
	
EWXWEXPORT(int, wxListEvent_GetMask)(void* _obj)
{
	return (int)((wxListEvent*)_obj)->GetMask();
}
	
EWXWEXPORT(void, wxListEvent_GetItem)(void* _obj, void* _ref)
{
#if wxVERSION_NUMBER < 2400
	*((wxListItem*)_ref) = ((wxListEvent*)_obj)->GetItem();
#else
	wxListItem* ret = new wxListItem(((wxListEvent*)_obj)->GetItem());
	*((void**)_ref) = (void*)ret;
#endif
}
	
EWXWEXPORT(void, wxTreeEvent_GetItem)(void* _obj, void* _ref)
{
	*((wxTreeItemId*)_ref) = ((wxTreeEvent*)_obj)->GetItem();
}
	
EWXWEXPORT(void, wxTreeEvent_GetOldItem)(void* _obj, void* _ref)
{
	*((wxTreeItemId*)_ref) = ((wxTreeEvent*)_obj)->GetOldItem();
}
	
EWXWEXPORT(void, wxTreeEvent_GetPoint)(void* _obj, void* x, void* y)
{
	wxPoint pos = ((wxTreeEvent*)_obj)->GetPoint();
	*((int*)x) = pos.x;
	*((int*)y) = pos.y;
}
	
EWXWEXPORT(int, wxTreeEvent_GetCode)(void* _obj)
{
#if wxVERSION_NUMBER < 2400
	return ((wxTreeEvent*)_obj)->GetCode();
#else
	return ((wxTreeEvent*)_obj)->GetKeyCode();
#endif
}
	
EWXWEXPORT(int, wxTreeEvent_GetLabel)(void* _obj, void* _buf)
{
	wxString result = ((wxTreeEvent*)_obj)->GetLabel();
	return copyStrToBuf(_buf, result);
}
	
EWXWEXPORT(int, wxSpinEvent_GetPosition)(void* _obj)
{
	return ((wxSpinEvent*)_obj)->GetPosition();
}
	
EWXWEXPORT(void, wxSpinEvent_SetPosition)(void* _obj, int pos)
{
	((wxSpinEvent*)_obj)->SetPosition(pos);
}
	
EWXWEXPORT(int, wxTimerEvent_GetInterval)(void* _obj)
{
	return ((wxTimerEvent*)_obj)->GetInterval();
}
	
EWXWEXPORT(int, wxCalendarEvent_GetWeekDay)(void* _obj)
{
	return ((wxCalendarEvent*)_obj)->GetWeekDay();
}

EWXWEXPORT(void, wxCalendarEvent_GetDate)(void* _obj, void* _dte)
{
	*((wxDateTime*)_dte) = ((wxCalendarEvent*)_obj)->GetDate();
}


EWXWEXPORT(int, wxScrollEvent_GetOrientation)(void* _obj)
{
	return ((wxScrollEvent*)_obj)->GetOrientation();
}
	
EWXWEXPORT(int, wxScrollEvent_GetPosition)(void* _obj)
{
	return ((wxScrollEvent*)_obj)->GetPosition();
}
	
#if wxVERSION_NUMBER >= 2400

EWXWEXPORT(void,wxHelpEvent_GetPosition)(void* _obj, void* x, void* y)
{
	wxPoint pos = ((wxHelpEvent*)_obj)->GetPosition();
	*((int*)x) = pos.x;
	*((int*)y) = pos.y;
}
	
EWXWEXPORT(void,wxHelpEvent_SetPosition)(void* _obj, int x, int y)
{
	((wxHelpEvent*)_obj)->SetPosition(wxPoint(x, y));
}
	
EWXWEXPORT(int,wxHelpEvent_GetLink)(void* _obj, void* _ref)
{
	wxString tmp =((wxHelpEvent*)_obj)->GetLink();
        return copyStrToBuf(_ref, tmp);
}
	
EWXWEXPORT(void,wxHelpEvent_SetLink)(void* _obj, void* link)
{
	((wxHelpEvent*)_obj)->SetLink((char*)link);
}
	
EWXWEXPORT(int,wxHelpEvent_GetTarget)(void* _obj, void* _ref)
{
	wxString tmp =((wxHelpEvent*)_obj)->GetTarget();
        return copyStrToBuf(_ref, tmp);
}
	
EWXWEXPORT(void,wxHelpEvent_SetTarget)(void* _obj, void* target)
{
	((wxHelpEvent*)_obj)->SetTarget((char*)target);
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
	return (int)wxEVT_POWER;
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
	return (int)wxEVT_DIALUP_CONNECTED;
}

EWXWEXPORT(int,expEVT_DIALUP_DISCONNECTED)()
{
	return (int)wxEVT_DIALUP_DISCONNECTED;
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

EWXWEXPORT(int,expEVT_COMMAND_LIST_GET_INFO)()
{
	return (int)wxEVT_COMMAND_LIST_GET_INFO;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_SET_INFO)()
{
	return (int)wxEVT_COMMAND_LIST_SET_INFO;
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
#ifdef __WXGTK__
	return -1;
#else
	return (int)wxEVT_COMMAND_TAB_SEL_CHANGED;
#endif
}

EWXWEXPORT(int,expEVT_COMMAND_TAB_SEL_CHANGING)()
{
#ifdef __WXGTK__
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

#endif

}
