#include "wrapper.h"
#include "wx/joystick.h"

extern "C"
{

#if wxUSE_JOYSTICK

EWXWEXPORT(void*,wxJoystick_Create)(int joystick)
{
	return (void*)new wxJoystick(joystick);
}

EWXWEXPORT(void,wxJoystick_Delete)(void* _obj)
{
	delete (wxJoystick*)_obj;
}

EWXWEXPORT(void,wxJoystick_GetPosition)(void* _obj, void* _x, void* _y)
{
	wxPoint pt = ((wxJoystick*)_obj)->GetPosition();
	*((int*)_x) = pt.x;
	*((int*)_y) = pt.y;
}
	
EWXWEXPORT(int,wxJoystick_GetZPosition)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetZPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetButtonState)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetButtonState();
}
	
EWXWEXPORT(int,wxJoystick_GetPOVPosition)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetPOVPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetPOVCTSPosition)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetPOVCTSPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetRudderPosition)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetRudderPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetUPosition)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetUPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetVPosition)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetVPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetMovementThreshold)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetMovementThreshold();
}
	
EWXWEXPORT(void,wxJoystick_SetMovementThreshold)(void* _obj,int threshold)
{
	((wxJoystick*)_obj)->SetMovementThreshold(threshold);
}
	
EWXWEXPORT(bool,wxJoystick_IsOk)(wxJoystick* _obj)
{
	return _obj->IsOk();
}
	
EWXWEXPORT(int,wxJoystick_GetNumberJoysticks)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetNumberJoysticks();
}
	
EWXWEXPORT(int,wxJoystick_GetManufacturerId)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetManufacturerId();
}
	
EWXWEXPORT(int,wxJoystick_GetProductId)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetProductId();
}
	
EWXWEXPORT(wxString*,wxJoystick_GetProductName)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxJoystick*)_obj)->GetProductName();
	return result;
}
	
EWXWEXPORT(int,wxJoystick_GetXMin)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetXMin();
}
	
EWXWEXPORT(int,wxJoystick_GetYMin)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetYMin();
}
	
EWXWEXPORT(int,wxJoystick_GetZMin)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetZMin();
}
	
EWXWEXPORT(int,wxJoystick_GetXMax)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetXMax();
}
	
EWXWEXPORT(int,wxJoystick_GetYMax)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetYMax();
}
	
EWXWEXPORT(int,wxJoystick_GetZMax)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetZMax();
}
	
EWXWEXPORT(int,wxJoystick_GetNumberButtons)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetNumberButtons();
}
	
EWXWEXPORT(int,wxJoystick_GetNumberAxes)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetNumberAxes();
}
	
EWXWEXPORT(int,wxJoystick_GetMaxButtons)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetMaxButtons();
}
	
EWXWEXPORT(int,wxJoystick_GetMaxAxes)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetMaxAxes();
}
	
EWXWEXPORT(int,wxJoystick_GetPollingMin)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetPollingMin();
}
	
EWXWEXPORT(int,wxJoystick_GetPollingMax)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetPollingMax();
}
	
EWXWEXPORT(int,wxJoystick_GetRudderMin)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetRudderMin();
}
	
EWXWEXPORT(int,wxJoystick_GetRudderMax)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetRudderMax();
}
	
EWXWEXPORT(int,wxJoystick_GetUMin)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetUMin();
}
	
EWXWEXPORT(int,wxJoystick_GetUMax)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetUMax();
}
	
EWXWEXPORT(int,wxJoystick_GetVMin)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetVMin();
}
	
EWXWEXPORT(int,wxJoystick_GetVMax)(void* _obj)
{
	return ((wxJoystick*)_obj)->GetVMax();
}
	
EWXWEXPORT(bool,wxJoystick_HasRudder)(wxJoystick* _obj)
{
	return _obj->HasRudder();
}
	
EWXWEXPORT(bool,wxJoystick_HasZ)(wxJoystick* _obj)
{
	return _obj->HasZ();
}
	
EWXWEXPORT(bool,wxJoystick_HasU)(wxJoystick* _obj)
{
	return _obj->HasU();
}
	
EWXWEXPORT(bool,wxJoystick_HasV)(wxJoystick* _obj)
{
	return _obj->HasV();
}
	
EWXWEXPORT(bool,wxJoystick_HasPOV)(wxJoystick* _obj)
{
	return _obj->HasPOV();
}
	
EWXWEXPORT(bool,wxJoystick_HasPOV4Dir)(wxJoystick* _obj)
{
	return _obj->HasPOV4Dir();
}
	
EWXWEXPORT(bool,wxJoystick_HasPOVCTS)(wxJoystick* _obj)
{
	return _obj->HasPOVCTS();
}
	
EWXWEXPORT(bool,wxJoystick_SetCapture)(wxJoystick* _obj,wxWindow* win,int pollingFreq)
{
	return _obj->SetCapture(win, pollingFreq);
}
	
EWXWEXPORT(bool,wxJoystick_ReleaseCapture)(wxJoystick* _obj)
{
	return _obj->ReleaseCapture();
}

#else

EWXWEXPORT(void*,wxJoystick_Create)(int joystick)
{
	return NULL;
}

EWXWEXPORT(void,wxJoystick_Delete)(void* _obj)
{
}

EWXWEXPORT(void,wxJoystick_GetPosition)(void* _obj,void* _x,void* _y)
{
}
	
EWXWEXPORT(int,wxJoystick_GetZPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetButtonState)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetPOVPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetPOVCTSPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetRudderPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetUPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetVPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetMovementThreshold)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(void,wxJoystick_SetMovementThreshold)(void* _obj,int threshold)
{
}
	
EWXWEXPORT(bool,wxJoystick_IsOk)(wxJoystick* _obj)
{
	return _obj->isOk();
}
	
EWXWEXPORT(int,wxJoystick_GetNumberJoysticks)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetManufacturerId)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetProductId)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetProductName)(void* _obj,void* _buf)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetXMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetYMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetZMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetXMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetYMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetZMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetNumberButtons)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetNumberAxes)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetMaxButtons)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetMaxAxes)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetPollingMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetPollingMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetRudderMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetRudderMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetUMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetUMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetVMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetVMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasRudder)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasZ)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasU)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasV)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasPOV)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasPOV4Dir)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasPOVCTS)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_SetCapture)(void* _obj,void* win,int pollingFreq)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_ReleaseCapture)(void* _obj)
{
	return 0;
}

#endif
	
}
