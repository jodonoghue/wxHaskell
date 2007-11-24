#ifdef wxUSE_STC
#include "wx/stc/stc.h"
#endif

#include "wrapper.h"


extern "C"
{
#include "stc_gen.cpp"

/* wxStyledTextCtrl */

EWXWEXPORT(void*, wxStyledTextCtrl_Create) (void* _prt, int _id, wxChar* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
#ifdef wxUSE_STC
  return (void*) new wxStyledTextCtrl((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, _txt);
#else
  return false;
#endif
}

  /* tricky handwritten functions */

  EWXWEXPORT(void*, wxStyledTextCtrl_IndicatorGetForeground) (void* _obj, int indic)
  {
#ifdef wxUSE_STC
    wxColour c = ((wxStyledTextCtrl*) _obj)->IndicatorGetForeground(indic);
    wxColour* cc = new wxColour(c);
    return cc;
#else
    return false;
#endif
  }
  EWXWEXPORT(void*, wxStyledTextCtrl_GetCaretLineBack) (void* _obj)
  {
#ifdef wxUSE_STC
    wxColour c = ((wxStyledTextCtrl*) _obj)->GetCaretLineBack();
    wxColour* cc = new wxColour(c);
    return cc;
#else
    return false;
#endif
  }
  EWXWEXPORT(void*, wxStyledTextCtrl_GetCaretForeground) (void* _obj)
  {
#ifdef wxUSE_STC
    wxColour c = ((wxStyledTextCtrl*) _obj)->GetCaretForeground();
    wxColour* cc = new wxColour(c);
    return cc;
#else
    return false;
#endif
  }
  EWXWEXPORT(void*, wxStyledTextCtrl_GetLine) (void* _obj, int line)
  {
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextCtrl*) _obj)->GetLine(line);
    wxString* ss = new wxString(s);
    return ss;
#else
    return false;
#endif
  }

  EWXWEXPORT(void*, wxStyledTextCtrl_GetText) (void* _obj)
  {
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextCtrl*) _obj)->GetText();
    wxString* ss = new wxString(s);
    return ss;
#else
    return false;
#endif
  }

  EWXWEXPORT(void*, wxStyledTextCtrl_GetSelectedText) (void* _obj)
  {
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextCtrl*) _obj)->GetSelectedText();
    wxString* ss = new wxString(s);
    return ss;
#else
    return false;
#endif
  }

  EWXWEXPORT(void*, wxStyledTextCtrl_GetTextRange) (void* _obj, int startPos, int endPos)
  {
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextCtrl*) _obj)->GetTextRange(startPos, endPos);
    wxString* ss = new wxString(s);
    return ss;
#else
    return false;
#endif
  }

  EWXWEXPORT(void*, wxStyledTextCtrl_CreateDocument) (void* _obj)
  {
#ifdef wxUSE_STC
    return ((wxStyledTextCtrl*) _obj)->CreateDocument();
#else
    return false;
#endif
  }

  EWXWEXPORT(void*, wxStyledTextCtrl_GetEdgeColour) (void* _obj)
  {
#ifdef wxUSE_STC
    wxColour c = ((wxStyledTextCtrl*) _obj)->GetEdgeColour();
    wxColour* cc = new wxColour(c);
    return cc;
#else
    return false;
#endif
  }

  EWXWEXPORT(void*, wxStyledTextCtrl_GetDocPointer) (void* _obj)
  {
#ifdef wxUSE_STC
    return ((wxStyledTextCtrl*) _obj)->GetDocPointer();
#else
    return false;
#endif
  }

  EWXWEXPORT(void*, wxStyledTextCtrl_PointFromPosition) (void* _obj, int pos)
  {
#ifdef wxUSE_STC
    wxPoint p = ((wxStyledTextCtrl*) _obj)->PointFromPosition(pos);
    wxPoint* pp = new wxPoint(p);
    return pp;
#else
    return false;
#endif
  }

  /*
("wxMemoryBuffer","GetStyledText",[("int","startPos"),("int","endPos")])
("wxString","GetCurLine",[("int*","linePos")]) #returns both line and pos, hur göra?
--("wxColour","IndicatorGetForeground",[("int","indic")])
--("wxColour","GetCaretLineBack",[])
--("wxColour","GetCaretForeground",[])
--("wxString","GetLine",[("int","line")])
--("wxString","GetSelectedText",[])
--("wxString","GetTextRange",[("int","startPos"),("int","endPos")])
--("wxString","GetText",[])
--("wxSTCDoc*","GetDocPointer",[])
--("wxColour","GetEdgeColour",[])
--("wxSTCDoc*","CreateDocument",[])
--("wxPoint","PointFromPosition",[("int","pos")])
*/


/*************************************/
/* wxStyledTextEvent's get functions */
/*************************************/

EWXWEXPORT(int, wxStyledTextEvent_GetPosition) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetPosition();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetKey) (void* _obj)
{
#ifdef wxUSE_STC
  return (char)((wxStyledTextEvent*) _obj)->GetKey();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetModifiers) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetModifiers();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetModificationType) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetModificationType();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetLength) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetLength();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetLinesAdded) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetLinesAdded();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetLine) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetLine();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetFoldLevelNow) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetFoldLevelNow();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetFoldLevelPrev) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetFoldLevelPrev();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetMargin) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetMargin();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetMessage) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetMessage();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetWParam) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetWParam();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetLParam) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetLParam();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetListType) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetListType();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetX) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetX();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetY) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetY();
#else
  return false;
#endif
}

EWXWEXPORT(void*, wxStyledTextEvent_GetDragText) (void* _obj)
{
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextEvent*) _obj)->GetDragText();
    wxString* ss = new wxString(s);
    return ss;
#else
  return false;
#endif
}

EWXWEXPORT(bool, wxStyledTextEvent_GetDragAllowMove) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetDragAllowMove();
#else
  return false;
#endif
}

EWXWEXPORT(int, wxStyledTextEvent_GetDragResult) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetDragResult();
#else
  return false;
#endif
}

EWXWEXPORT(bool, wxStyledTextEvent_GetShift) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetShift();
#else
  return false;
#endif
}

EWXWEXPORT(bool, wxStyledTextEvent_GetControl) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetControl();
#else
  return false;
#endif
}

EWXWEXPORT(bool, wxStyledTextEvent_GetAlt) (void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetAlt();
#else
  return false;
#endif
}

EWXWEXPORT(void*, wxStyledTextEvent_GetText) (void* _obj)
{
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextEvent*) _obj)->GetText();
    wxString* ss = new wxString(s);
    return ss;
#else
  return false;
#endif
}

EWXWEXPORT(void*, wxStyledTextEvent_Clone) (void* _obj)
{
#ifdef wxUSE_STC
  return (void*) ((wxStyledTextEvent*) _obj)->Clone();
#else
  return false;
#endif
}

/*************************************/
/* wxStyledTextEvent's set functions */
/*************************************/

EWXWEXPORT(void, wxStyledTextEvent_SetPosition) (void* _obj, int pos)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetPosition(pos);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetKey) (void* _obj, int k)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetKey(k);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetModifiers) (void* _obj, int m)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetModifiers(m);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetModificationType) (void* _obj, int t)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetModificationType(t);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetText) (void* _obj, void* t)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetText(*(wxString*)t);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetLength) (void* _obj, int len)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetLength(len);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetLinesAdded) (void* _obj, int num)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetLinesAdded(num);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetLine) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetLine(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetFoldLevelNow) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetFoldLevelNow(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetFoldLevelPrev) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetFoldLevelPrev(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetMargin) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetMargin(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetMessage) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetMessage(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetWParam) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetWParam(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetLParam) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetLParam(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetListType) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetListType(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetX) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetX(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetY) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetY(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetDragText) (void* _obj, void* val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetDragText(*(wxString*)val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetDragAllowMove) (void* _obj, int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetDragAllowMove(val);
#endif
}

EWXWEXPORT(void, wxStyledTextEvent_SetDragResult) (void* _obj, void* val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetDragResult(*(wxDragResult*)val);
#else
  return false;
#endif
}

/*************************************/
/* wxStyledTextEvent events          */
/*************************************/
EWXWEXPORT(int, expEVT_STC_CHANGE)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_CHANGE;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_STYLENEEDED)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_STYLENEEDED;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_CHARADDED)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_CHARADDED;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_SAVEPOINTREACHED)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_SAVEPOINTREACHED;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_SAVEPOINTLEFT)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_SAVEPOINTLEFT;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_ROMODIFYATTEMPT)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_ROMODIFYATTEMPT;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_KEY)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_KEY;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_DOUBLECLICK)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_DOUBLECLICK;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_UPDATEUI)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_UPDATEUI;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_MODIFIED)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_MODIFIED;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_MACRORECORD)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_MACRORECORD;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_MARGINCLICK)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_MARGINCLICK;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_NEEDSHOWN)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_NEEDSHOWN;
#else
  return false;
#endif
}
/* expEVT_STC_POSCHANGED is removed in wxWidgets-2.6.x.
EWXWEXPORT(int, expEVT_STC_POSCHANGED)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_POSCHANGED;
#else
  return false;
#endif
}
*/
EWXWEXPORT(int, expEVT_STC_PAINTED)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_PAINTED;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_USERLISTSELECTION)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_USERLISTSELECTION;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_URIDROPPED)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_URIDROPPED;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_DWELLSTART)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_DWELLSTART;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_DWELLEND)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_DWELLEND;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_START_DRAG)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_START_DRAG;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_DRAG_OVER)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_DRAG_OVER;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_DO_DROP)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_DO_DROP;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_ZOOM)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_ZOOM;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_HOTSPOT_CLICK)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_HOTSPOT_CLICK;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_HOTSPOT_DCLICK)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_HOTSPOT_DCLICK;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_CALLTIP_CLICK)()
{
#ifdef wxUSE_STC
  return wxEVT_STC_CALLTIP_CLICK;
#else
  return false;
#endif
}
EWXWEXPORT(int, expEVT_STC_AUTOCOMP_SELECTION)()
{
#if (wxVERSION_NUMBER >= 2600) && wxUSE_STC
  return wxEVT_STC_AUTOCOMP_SELECTION;
#else
  return false;
#endif
}

}
