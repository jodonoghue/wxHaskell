/*==============================================================================
 * taskbaricon.cpp
 *
 * C wrapper for wxTaskbarIcon
 *
 * (C) 2002-2011 wxEiffel and wxHaskell contributors. See contributors.txt
 * 
 * Licensed under the wxWidgets library license. see LICENSE.
 * 
 *==============================================================================*/

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

EWXWEXPORT(bool,wxTaskBarIcon_IsIconInstalled)(wxTaskBarIcon* self)
{
  return self->IsIconInstalled();
}

EWXWEXPORT(bool,wxTaskBarIcon_IsOk)(wxTaskBarIcon* self)
{
  return self->IsOk();
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
#if (wxVERSION_NUMBER < 2900)
  return self->SetIcon(*icon, (tooltip ? *tooltip : *wxEmptyString));
#else
  return tooltip ? self->SetIcon(*icon, *tooltip) : self->SetIcon(*icon);
#endif
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


