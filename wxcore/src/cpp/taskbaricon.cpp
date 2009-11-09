#include "wrapper.h"
#include <wx/taskbar.h>

extern "C" {
/*-----------------------------------------------------------------------------
  TaskBarIcon
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxTaskBarIcon*,wxTaskBarIcon_Create)()
{
  return new wxTaskBarIcon();
}

EWXWEXPORT(void,wxTaskBarIcon_Delete)(wxTaskBarIcon* self)
{
  if (self) delete self;
}

/*
EWXWEXPORT(wxMenu*,wxTaskBarIcon_CreatePopupMenu)(wxTaskBarIcon* self)
{
  return self->CreatePopupMenu();
}
*/

EWXWEXPORT(bool,wxTaskBarIcon_IsIconInstalled)(wxTaskBarIcon* self)
{
  return self->IsIconInstalled();
}

EWXWEXPORT(bool,wxTaskBarIcon_IsOk)(wxTaskBarIcon* self)
{
#if (wxVERSION_NUMBER >= 2600)
  return self->IsOk();
#else
  return self->IsOK();
#endif
}

EWXWEXPORT(bool,wxTaskBarIcon_PopupMenu)(wxTaskBarIcon* self,wxMenu* menu)
{
  return self->PopupMenu(menu);
}

EWXWEXPORT(bool,wxTaskBarIcon_RemoveIcon)(wxTaskBarIcon* self)
{
  return self->RemoveIcon();
}

EWXWEXPORT(bool,wxTaskBarIcon_SetIcon)(wxTaskBarIcon* self,wxIcon* icon,wxString* tooltip)
{
  return self->SetIcon(*icon, (tooltip ? *tooltip : *wxEmptyString));
}

EWXWEXPORT(int,expEVT_TASKBAR_MOVE)()
{
    return (int)wxEVT_TASKBAR_MOVE;
}

EWXWEXPORT(int,expEVT_TASKBAR_LEFT_DOWN)()
{
    return (int)wxEVT_TASKBAR_LEFT_DOWN;
}

EWXWEXPORT(int,expEVT_TASKBAR_LEFT_UP)()
{
    return (int)wxEVT_TASKBAR_LEFT_UP;
}

EWXWEXPORT(int,expEVT_TASKBAR_RIGHT_DOWN)()
{
    return (int)wxEVT_TASKBAR_RIGHT_DOWN;
}

EWXWEXPORT(int,expEVT_TASKBAR_RIGHT_UP)()
{
    return (int)wxEVT_TASKBAR_RIGHT_UP;
}

EWXWEXPORT(int,expEVT_TASKBAR_LEFT_DCLICK)()
{
    return (int)wxEVT_TASKBAR_LEFT_DCLICK;
}

EWXWEXPORT(int,expEVT_TASKBAR_RIGHT_DCLICK)()
{
    return (int)wxEVT_TASKBAR_RIGHT_DCLICK;
}

}


