#include "wrapper.h"
#include "wx/process.h"
#include "wx/textctrl.h"
#include "wx/progdlg.h"
#include "wx/listctrl.h"
#include "wx/grid.h"
#include "wx/fileconf.h"
#include "wx/spinctrl.h"

#if (wxVERSION_NUMBER >= 2800)
#include <wx/numdlg.h>
#include <wx/power.h>
#endif

/*-----------------------------------------------------------------------------
  new events
-----------------------------------------------------------------------------*/
BEGIN_DECLARE_EVENT_TYPES()
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_DELETE, 1000)
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_HTML_CELL_CLICKED, 1001 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_HTML_CELL_MOUSE_HOVER, 1002 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_HTML_LINK_CLICKED, 1003 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_HTML_SET_TITLE, 1004 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_INPUT_SINK, 1005 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_SORT, 1006 )
END_DECLARE_EVENT_TYPES()


DEFINE_LOCAL_EVENT_TYPE( wxEVT_DELETE )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_HTML_CELL_CLICKED )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_HTML_CELL_MOUSE_HOVER )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_HTML_LINK_CLICKED )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_HTML_SET_TITLE )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_INPUT_SINK )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_SORT )

/*-----------------------------------------------------------------------------
  event exports
-----------------------------------------------------------------------------*/
extern "C"
{

EWXWEXPORT(int,expEVT_DELETE)()
{
  return (int)wxEVT_DELETE;
}

EWXWEXPORT(int,expEVT_HTML_CELL_CLICKED)()
{
  return (int)wxEVT_HTML_CELL_CLICKED;
}

EWXWEXPORT(int,expEVT_HTML_CELL_MOUSE_HOVER)()
{
  return (int)wxEVT_HTML_CELL_MOUSE_HOVER;
}

EWXWEXPORT(int,expEVT_HTML_LINK_CLICKED)()
{
  return (int)wxEVT_HTML_LINK_CLICKED;
}


EWXWEXPORT(int,expEVT_HTML_SET_TITLE)()
{
  return (int)wxEVT_HTML_SET_TITLE;
}


EWXWEXPORT(int,expEVT_INPUT_SINK)()
{
  return (int)wxEVT_INPUT_SINK;
}


EWXWEXPORT(int,expEVT_SORT)()
{
  return (int)wxEVT_SORT;
}

/* list control */
EWXWEXPORT(int,expEVT_COMMAND_LIST_CACHE_HINT)()
{
  return (int)wxEVT_COMMAND_LIST_CACHE_HINT;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_COL_RIGHT_CLICK)()
{
  return (int)wxEVT_COMMAND_LIST_COL_RIGHT_CLICK;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_COL_BEGIN_DRAG)()
{
  return (int)wxEVT_COMMAND_LIST_COL_BEGIN_DRAG;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_COL_DRAGGING)()
{
  return (int)wxEVT_COMMAND_LIST_COL_DRAGGING;
}

EWXWEXPORT(int,expEVT_COMMAND_LIST_COL_END_DRAG)()
{
  return (int)wxEVT_COMMAND_LIST_COL_END_DRAG;
}

} /* extern "C" */


/*-----------------------------------------------------------------------------
  Timers
-----------------------------------------------------------------------------*/
class wxTimerEx : public wxTimer
{
private:
  wxClosure* m_closure;
public:
  wxTimerEx();
  ~wxTimerEx();

  void     Connect( wxClosure* closure );
  wxClosure* GetClosure();

  void Notify();
};

wxTimerEx::wxTimerEx()
{
  m_closure = NULL;
}

wxTimerEx::~wxTimerEx()
{
  if (m_closure) {
    m_closure->DecRef();
    m_closure=NULL;
  }
}

void wxTimerEx::Connect( wxClosure* closure )
{
  if (m_closure) m_closure->DecRef();
  m_closure = closure;
  if (m_closure) m_closure->IncRef();
}

wxClosure* wxTimerEx::GetClosure()
{
  return m_closure;
}

void wxTimerEx::Notify()
{
  wxTimerEvent timerEvent(0,this->GetInterval());
  if (m_closure) m_closure->Invoke(&timerEvent);
}

/*-----------------------------------------------------------------------------
  wxInputSink wrapper: adds non-blocking event driven input channel
-----------------------------------------------------------------------------*/
class wxInputSink;

class wxInputSinkEvent : public wxEvent
{
private:
  char*         m_buffer;
  size_t        m_bufferLen;
  size_t        m_lastRead;
  wxStreamError m_lastError;

  void Read( wxInputStream* input );
  friend class wxInputSink;

public:
  wxInputSinkEvent( int id, size_t bufferLen );
  wxInputSinkEvent( const wxInputSinkEvent& event );  /* copy constructor */
  ~wxInputSinkEvent();

  wxEvent* Clone() const          { return new wxInputSinkEvent(*this); }

  wxStreamError LastError() const { return m_lastError; }
  int           LastRead()  const { return m_lastRead; }
  char*         LastInput() const { return m_buffer;   }
};

class wxInputSink : public wxThread
{
private:
  wxEvtHandler*    m_evtHandler;
  wxInputStream*   m_input;
  wxInputSinkEvent m_event;

protected:
  ExitCode Entry();

public:
  wxInputSink( wxInputStream* input, wxEvtHandler* evtHandler, int bufferLen );
  ~wxInputSink();

  intptr_t GetId();
  void Start();
};


wxInputSink::wxInputSink( wxInputStream* input, wxEvtHandler* evtHandler, int bufferLen )
  : wxThread(wxTHREAD_DETACHED), m_event( 0, bufferLen )
{
  m_input      = input;
  m_evtHandler = evtHandler;
}

wxInputSink::~wxInputSink()
{
/* fprintf(stderr, "InputSink is deleted\n"); */
}

void wxInputSink::Start()
{
  wxThreadError result = Create();
  switch(result) {
    case wxTHREAD_NO_ERROR:  Run(); break;
    case wxTHREAD_RUNNING :  break;
    default               :  Delete(); break;
  }
}

intptr_t wxInputSink::GetId()
{
  return (intptr_t)m_input;
}

wxThread::ExitCode wxInputSink::Entry()
{
  if (m_input == NULL || m_evtHandler == NULL || m_event.m_buffer == NULL ) return 0;

  m_event.SetId(GetId());

  /* while input && not external destroy */
  while (!TestDestroy() && m_event.LastError() == wxSTREAM_NO_ERROR)
  {
     /* (blocking) read */
     m_event.Read( m_input );

     /* post the event to the main gui thread (note: event is cloned and thus the input buffer copied)*/
     m_evtHandler->AddPendingEvent(m_event);
  }

  /* Process pending events */
  wxWakeUpIdle();

  /* return */
  if (m_event.LastError() == wxSTREAM_NO_ERROR || m_event.LastError() == wxSTREAM_EOF)
     return (ExitCode)0;
  else
     return (ExitCode)1;
}


wxInputSinkEvent::wxInputSinkEvent( int id, size_t bufferLen ) : wxEvent( id, wxEVT_INPUT_SINK  )
{
  m_lastError = wxSTREAM_NO_ERROR;
  m_lastRead  = 0;
  if (bufferLen <= 0) bufferLen = 128;
  m_buffer    = (char*)malloc( bufferLen + 1 );
  m_bufferLen = (m_buffer ? bufferLen : 0);
}

wxInputSinkEvent::wxInputSinkEvent( const wxInputSinkEvent& event ) : wxEvent( event )
{
  /* we copy only the exact input buffer, as 'Read' will never be called */
  m_lastError = event.m_lastError;
  m_bufferLen = 0;
  m_lastRead  = 0;
  m_buffer    = (char*)malloc( event.m_lastRead + 1 );
  if (m_buffer) {
    m_bufferLen = event.m_lastRead;
    m_lastRead  = event.m_lastRead;
    memcpy( m_buffer, event.m_buffer, m_lastRead );
    m_buffer[m_lastRead] = 0;
  }
}

wxInputSinkEvent::~wxInputSinkEvent()
{
  if (m_buffer) free(m_buffer);
}

void wxInputSinkEvent::Read( wxInputStream* input )
{
  /* check */
  if (input == NULL || m_buffer == NULL || m_bufferLen == 0) {
    m_lastError = wxSTREAM_READ_ERROR;
    m_lastRead  = 0;
    return;
  }

  /* read */
  input->Read( m_buffer, m_bufferLen );
#if wxCHECK_VERSION(2,5,0)
  m_lastError = input->GetLastError();
#else
  m_lastError = input->LastError();
#endif
  if (m_lastError == wxSTREAM_NO_ERROR)
    m_lastRead  = input->LastRead();
  else
    m_lastRead  = 0;


  /* maintain invariants */
  if (m_lastRead < 0)           m_lastRead = 0;
  if (m_lastRead > m_bufferLen) m_lastRead = m_bufferLen;

  /* add zero terminator */
  m_buffer[m_lastRead] = 0;
}


/*-----------------------------------------------------------------------------
  wxHtmlWindow wrapper: adds normal events instead of using inheritance
-----------------------------------------------------------------------------*/
class wxcHtmlWindow : public wxHtmlWindow
{
private:
    DECLARE_DYNAMIC_CLASS(wxcHtmlWindow)
public:
    wxcHtmlWindow() : wxHtmlWindow() {};   wxcHtmlWindow(wxWindow *, wxWindowID id, const wxPoint&, const wxSize& size, long style, const wxString& );

#if (wxVERSION_NUMBER <= 2800)
   void OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event);
#else
   bool OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event);
#endif
   void OnCellMouseHover(wxHtmlCell *cell, wxCoord x, wxCoord y);
   void OnLinkClicked(const wxHtmlLinkInfo& link);
   wxHtmlOpeningStatus OnOpeningURL(wxHtmlURLType type,const wxString& url, wxString *redirect);
   void OnSetTitle(const wxString& title);
};

IMPLEMENT_DYNAMIC_CLASS(wxcHtmlWindow, wxHtmlWindow)

class wxcHtmlEvent : public wxCommandEvent
{
private:
    DECLARE_DYNAMIC_CLASS(wxcHtmlEvent)
private:
    const wxMouseEvent*     m_mouseEvent;
    const wxHtmlCell*       m_htmlCell;
    const wxHtmlLinkInfo*   m_linkInfo;
    wxPoint                 m_logicalPosition;

public:
    wxcHtmlEvent() : wxCommandEvent() {};
    wxcHtmlEvent( wxEventType commandType, int id, const wxMouseEvent* mouseEvent, const wxHtmlCell* htmlCell, const wxHtmlLinkInfo* linkInfo, wxPoint logicalPoint );
    const wxMouseEvent* GetMouseEvent();
    const wxHtmlCell *  GetHtmlCell();
    wxString*           GetHtmlCellId();
    wxString*           GetHref();
    wxString*           GetTarget();
    wxPoint             GetLogicalPosition();
};

IMPLEMENT_DYNAMIC_CLASS(wxcHtmlEvent, wxCommandEvent)

wxcHtmlWindow::wxcHtmlWindow(wxWindow * prt, wxWindowID id, const wxPoint& pt, const wxSize& size, long style, const wxString& title )
: wxHtmlWindow( prt, id, pt, size, style, title )
{}

#if (wxVERSION_NUMBER < 2800)
void wxcHtmlWindow::OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event)
#else
bool wxcHtmlWindow::OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event)
#endif
{
    wxHtmlLinkInfo* linkPtr;
#if (wxVERSION_NUMBER < 2800)
    if (cell==NULL) return;
#else
    if (cell==NULL) return false;
#endif

    linkPtr = cell->GetLink(x, y);
    if (linkPtr != NULL)
    {
        wxHtmlLinkInfo link(*linkPtr);
        link.SetEvent(&event);
        link.SetHtmlCell(cell);
        {
          wxcHtmlEvent htmlEvent( wxEVT_HTML_LINK_CLICKED, this->GetId(), &event, cell, &link, wxPoint(x,y) );
          this->ProcessEvent( htmlEvent );
        }
    }
    else
    {
      wxcHtmlEvent htmlEvent( wxEVT_HTML_CELL_CLICKED, this->GetId(), &event, cell, NULL, wxPoint(x,y) );
      this->ProcessEvent( htmlEvent );
    }
#if (wxVERSION_NUMBER >= 2800)
    return true;
#endif
}

void wxcHtmlWindow::OnCellMouseHover(wxHtmlCell *cell, wxCoord x, wxCoord y)
{
    wxcHtmlEvent htmlEvent( wxEVT_HTML_CELL_MOUSE_HOVER, this->GetId(), NULL, cell, NULL, wxPoint(x,y) );
    this->ProcessEvent( htmlEvent );
}

void wxcHtmlWindow::OnLinkClicked(const wxHtmlLinkInfo& link)
{
    wxcHtmlEvent htmlEvent( wxEVT_HTML_LINK_CLICKED, this->GetId(), link.GetEvent(), link.GetHtmlCell(), &link, wxPoint(-1,-1) );
    this->ProcessEvent( htmlEvent );
}

void wxcHtmlWindow::OnSetTitle(const wxString& title)
{
    wxcHtmlEvent htmlEvent( wxEVT_HTML_SET_TITLE, this->GetId(), NULL, NULL, NULL, wxPoint(-1,-1) );
    htmlEvent.SetString( title );
    this->ProcessEvent( htmlEvent );
}


wxcHtmlEvent::wxcHtmlEvent( wxEventType commandType, int id, const wxMouseEvent* mouseEvent, const wxHtmlCell* htmlCell, const wxHtmlLinkInfo* linkInfo, wxPoint logicalPoint )
: wxCommandEvent( commandType, id )
{
    m_mouseEvent = mouseEvent;
    m_htmlCell = htmlCell;
    m_linkInfo = linkInfo;
    m_logicalPosition = logicalPoint;
}

const wxMouseEvent* wxcHtmlEvent::GetMouseEvent()
{
    return m_mouseEvent;
}

const wxHtmlCell *  wxcHtmlEvent::GetHtmlCell()
{
    return m_htmlCell;
}

wxString* wxcHtmlEvent::GetHtmlCellId()
{
    if (m_htmlCell)
        return new wxString(m_htmlCell->GetId());
    else
        return new wxString(wxT(""));
}

wxString* wxcHtmlEvent::GetHref()
{
    if (m_linkInfo)
        return new wxString(m_linkInfo->GetHref());
    else
        return new wxString(wxT(""));
}

wxString* wxcHtmlEvent::GetTarget()
{
    if (m_linkInfo)
        return new wxString (m_linkInfo->GetTarget());
    else
        return new wxString(wxT(""));
}

wxPoint wxcHtmlEvent::GetLogicalPosition()
{
    return m_logicalPosition;
}

/*-----------------------------------------------------------------------------
  wxGridCellTextEnterEditor
-----------------------------------------------------------------------------*/
class wxGridCellTextEnterEditor : public wxGridCellTextEditor
{
public:
  wxGridCellTextEnterEditor() : wxGridCellTextEditor() {}

  void Create(wxWindow* parent,
                        wxWindowID id,
                        wxEvtHandler* evtHandler);

};

void wxGridCellTextEnterEditor::Create(wxWindow* parent,
                                       wxWindowID id,
                                       wxEvtHandler* evtHandler)
{
    wxGridCellTextEditor::Create(parent, id, evtHandler);
    
    {
      long style = m_control->GetWindowStyle();
      m_control->SetWindowStyle( style | wxTE_PROCESS_ENTER );
    }
}


/*-----------------------------------------------------------------------------
  pre processor definitions
-----------------------------------------------------------------------------*/
static const wxChar* defineDefs[] = {
#ifdef __WINDOWS__
  wxT("__WINDOWS__"),
#endif // any Windows, yom may also use
#ifdef __WIN16__
  wxT("__WIN16__"),
#endif // Win16 API
#ifdef __WIN32__
  wxT("__WIN32__"),
#endif // Win32 API
#ifdef __WIN95__
  wxT("__WIN95__"),
#endif // Windows 95 or NT 4.0 and above system (not NT 3.5x)
#ifdef __WXGTK__
  wxT("__WXGTK__"),
#endif // GTK
#ifdef __WXGTK12__
  wxT("__WXGTK12__"),
#endif // GTK 1.2 or higher
#ifdef __WXGTK20__
  wxT("__WXGTK20__"),
#endif // GTK 2.0 or higher
#ifdef __WXMOTIF__
  wxT("__WXMOTIF__"),
#endif // Motif
#ifdef __WXMAC__
  wxT("__WXMAC__"),
#endif // MacOS
#ifdef __WXMGL__
  wxT("__WXMGL__"),
#endif // SciTech Soft MGL
#ifdef __WXUNIVERSAL__
  wxT("__WXUNIVERSAL__"),
#endif //will be also defined)
#ifdef __WXMSW__
  wxT("__WXMSW__"),
#endif // Any Windows
#ifdef __WXOS2__
  wxT("__WXOS2__"),
#endif // OS/2 native Presentation Manager
#ifdef __WXPM__
  wxT("__WXPM__"),
#endif // OS/2 native Presentation Manager
#ifdef __WXSTUBS__
  wxT("__WXSTUBS__"),
#endif // Stubbed version ('template' wxWin implementation)
#ifdef __WXXT__
  wxT("__WXXT__"),
#endif // Xt; mutually exclusive with WX_MOTIF, not implemented in wxWindows 2.x
// wxX11
#ifdef __WXX11__
  wxT("__WXX11__"),
#endif
#ifdef __WXUNIVERSAL__
  wxT("__WXUNIVERSAL__"),
#endif //will be also defined)
#ifdef __WXWINE__
  wxT("__WXWINE__"),
#endif // WINE (i.e. Win32 on Unix)
#ifdef __WXUNIVERSAL__
  wxT("__WXUNIVERSAL__"),
#endif // wxUniversal port, always defined in addition to one of the symbols above so this should be tested first.
#ifdef __X__
  wxT("__X__"),
#endif // any X11-based GUI toolkit except GTK+

// any Mac OS version
#ifdef __APPLE__
  wxT("__APPLE__"),
#endif
// AIX
#ifdef __AIX__
  wxT("__AIX__"),
#endif
// Any BSD
#ifdef __BSD__
  wxT("__BSD__"),
#endif
// Cygwin: Unix on Win32
#ifdef __CYGWIN__
  wxT("__CYGWIN__"),
#endif
// Mac OS X
#ifdef __DARWIN__
  wxT("__DARWIN__"),
#endif
#ifdef ____DATA_GENERAL____
  wxT("__DATA_GENERAL__"),
#endif
// DOS (used with wxMGL only)
#ifdef __DOS_GENERAL__
  wxT("__DOS_GENERAL__"),
#endif
// FreeBSD
#ifdef __FREEBSD__
  wxT("__FREEBSD__"),
#endif
// HP-UX (Unix)
#ifdef ____HPUX____
  wxT("__HPUX__"),
#endif
// Linux
#ifdef __LINUX__
  wxT("__LINUX__"),
#endif
// OSF/1
#ifdef __OSF__
  wxT("__OSF__"),
#endif
// IRIX
#ifdef __SGI__
  wxT("__SGI__"),
#endif
// Solaris
#ifdef __SOLARIS__
  wxT("__SOLARIS__"),
#endif
// Any Sun
#ifdef __SUN__
  wxT("__SUN__"),
#endif
// Sun OS
#ifdef __SUNOS__
  wxT("__SUNOS__"),
#endif
// SystemV R4
#ifdef __SVR4__
  wxT("__SVR4__"),
#endif
// SystemV generic
#ifdef __SYSV__
  wxT("__SYSV__"),
#endif
// Ultrix
#ifdef __ULTRIX__
  wxT("__ULTRIX__"),
#endif
// any Unix
#ifdef __UNIX__
  wxT("__UNIX__"),
#endif
// Unix, BeOS or VMS
#ifdef __UNIX_LIKE__
  wxT("__UNIX_LIKE__"),
#endif
// VMS
#ifdef __VMS__
  wxT("__VMS__"),
#endif
// any Windows
#ifdef __WINDOWS__
  wxT("__WINDOWS__"),
#endif

// DEC Alpha architecture
#ifdef __ALPHA__
  wxT("__ALPHA__"),
#endif
// Intel i386 or compatible
#ifdef __INTEL__
  wxT("__INTEL__"),
#endif
// Motorola Power PC
#ifdef __POWERPC__
  wxT("__POWERPC__"),
#endif

// Borland C++
#ifdef __BORLANDC__
  wxT("__BORLANDC__"),
#endif
// DJGPP
#ifdef __DJGPP__
  wxT("__DJGPP__"),
#endif
// Gnu C++ on any platform
#ifdef __GNUG__
  wxT("__GNUG__"),
#endif
// GnuWin32 compiler
#ifdef __GNUWIN32__
  wxT("__GNUWIN32__"),
#endif
// CodeWarrior MetroWerks compiler
#ifdef __MWERKS__
  wxT("__MWERKS__"),
#endif
// Sun CC
#ifdef __SUNCC__
  wxT("__SUNCC__"),
#endif
// Symantec C++
#ifdef __SYMANTECC__
  wxT("__SYMANTECC__"),
#endif
// IBM Visual Age (OS/2)
#ifdef __VISAGECPP__
  wxT("__VISAGECPP__"),
#endif
// Microsoft Visual C++
#ifdef __VISUALC__
  wxT("__VISUALC__"),
#endif
// AIX compiler
#ifdef __XLC__
  wxT("__XLC__"),
#endif
// Watcom C++
#ifdef __WATCOMC__
  wxT("__WATCOMC__"),
#endif
  NULL
};

static const wxChar* useDefs[] = {
#ifdef wxUSE_UNIX
  wxT("UNIX"),
#endif
#ifdef wxUSE_NANOX
  wxT("NANOX"),
#endif
#ifdef wxUSE_NATIVE_STATUSBAR
  wxT("NATIVE_STATUSBAR"),
#endif
#ifdef wxUSE_OWNER_DRAWN
  wxT("OWNER_DRAWN"),
#endif
#ifdef wxUSE_OWNER_DRAWN
  wxT("OWNER_DRAWN"),
#endif
#ifdef wxUSE_RICHEDIT
  wxT("RICHEDIT"),
#endif
#ifdef wxUSE_RICHEDIT
  wxT("RICHEDIT"),
#endif
#ifdef wxUSE_REGEX
  wxT("REGEX"),
#endif
#ifdef wxUSE_ZLIB
  wxT("ZLIB"),
#endif
#ifdef wxUSE_LIBPNG
  wxT("LIBPNG"),
#endif
#ifdef wxUSE_LIBJPEG
  wxT("LIBJPEG"),
#endif
#ifdef wxUSE_LIBTIFF
  wxT("LIBTIFF"),
#endif
#ifdef wxUSE_ODBC
  wxT("ODBC"),
#endif
#ifdef wxUSE_FREETYPE
  wxT("FREETYPE"),
#endif
#ifdef wxUSE_THREADS
  wxT("THREADS"),
#endif
#if defined(wxcREFUSE_OPENGL)
# undef wxUSE_OPENGL
# undef wxUSE_GLCANVAS
#endif
#ifdef wxUSE_OPENGL
  wxT("OPENGL"),
#endif
#ifdef wxUSE_GLCANVAS
  wxT("GLCANVAS"),
#endif
#ifdef wxUSE_GUI
  wxT("GUI"),
#endif
#ifdef wxUSE_NOGUI
  wxT("NOGUI"),
#endif
#ifdef wxUSE_ON_FATAL_EXCEPTION
  wxT("ON_FATAL_EXCEPTION"),
#endif
#ifdef wxUSE_SNGLINST_CHECKER
  wxT("SNGLINST_CHECKER"),
#endif
#ifdef wxUSE_CONSTRAINTS
  wxT("CONSTRAINTS"),
#endif
#ifdef wxUSE_VALIDATORS
  wxT("VALIDATORS"),
#endif
#ifdef wxUSE_CONTROLS
  wxT("CONTROLS"),
#endif
#ifdef wxUSE_POPUPWIN
  wxT("POPUPWIN"),
#endif
#ifdef wxUSE_TIPWINDOW
  wxT("TIPWINDOW"),
#endif
#ifdef wxUSE_ACCEL
  wxT("ACCEL"),
#endif
#ifdef wxUSE_CALENDARCTRL
  wxT("CALENDARCTRL"),
#endif
#ifdef wxUSE_FILEDLG
  wxT("FILEDLG"),
#endif
#ifdef wxUSE_FINDREPLDLG
  wxT("FINDREPLDLG"),
#endif
#ifdef wxUSE_FONTDLG
  wxT("FONTDLG"),
#endif
#ifdef wxUSE_MIMETYPE
  wxT("MIMETYPE"),
#endif
#ifdef wxUSE_SYSTEM_OPTIONS
  wxT("SYSTEM_OPTIONS"),
#endif
#ifdef wxUSE_MSGDLG
  wxT("MSGDLG"),
#endif
#ifdef wxUSE_NUMBERDLG
  wxT("NUMBERDLG"),
#endif
#ifdef wxUSE_TEXTDLG
  wxT("TEXTDLG"),
#endif
#ifdef wxUSE_STARTUP_TIPS
  wxT("STARTUP_TIPS"),
#endif
#ifdef wxUSE_PROGRESSDLG
  wxT("PROGRESSDLG"),
#endif
#ifdef wxUSE_CHOICEDLG
  wxT("CHOICEDLG"),
#endif
#ifdef wxUSE_COLOURDLG
  wxT("COLOURDLG"),
#endif
#ifdef wxUSE_DIRDLG
  wxT("DIRDLG"),
#endif
#ifdef wxUSE_DRAGIMAGE
  wxT("DRAGIMAGE"),
#endif
#ifdef wxUSE_PROPSHEET
  wxT("PROPSHEET"),
#endif
#ifdef wxUSE_WIZARDDLG
  wxT("WIZARDDLG"),
#endif
#ifdef wxUSE_SPLASH
  wxT("SPLASH"),
#endif
#ifdef wxUSE_JOYSTICK
  wxT("JOYSTICK"),
#endif
#ifdef wxUSE_BUTTON
  wxT("BUTTON"),
#endif
#ifdef wxUSE_CARET
  wxT("CARET"),
#endif
#ifdef wxUSE_BMPBUTTON
  wxT("BMPBUTTON"),
#endif
#ifdef wxUSE_CHECKBOX
  wxT("CHECKBOX"),
#endif
#ifdef wxUSE_CHECKLISTBOX
  wxT("CHECKLISTBOX"),
#endif
#ifdef wxUSE_COMBOBOX
  wxT("COMBOBOX"),
#endif
#ifdef wxUSE_CHOICE
  wxT("CHOICE"),
#endif
#ifdef wxUSE_GAUGE
  wxT("GAUGE"),
#endif
#ifdef wxUSE_GRID
  wxT("GRID"),
#endif
#ifdef wxUSE_NEW_GRID
  wxT("NEW_GRID"),
#endif
#ifdef wxUSE_IMAGLIST
  wxT("IMAGLIST"),
#endif
#ifdef wxUSE_LISTBOX
  wxT("LISTBOX"),
#endif
#ifdef wxUSE_LISTCTRL
  wxT("LISTCTRL"),
#endif
#ifdef wxUSE_MENUS
  wxT("MENUS"),
#endif
#ifdef wxUSE_NOTEBOOK
  wxT("NOTEBOOK"),
#endif
#ifdef wxUSE_RADIOBOX
  wxT("RADIOBOX"),
#endif
#ifdef wxUSE_RADIOBTN
  wxT("RADIOBTN"),
#endif
#ifdef wxUSE_SASH
  wxT("SASH"),
#endif
#ifdef wxUSE_SCROLLBAR
  wxT("SCROLLBAR"),
#endif
#ifdef wxUSE_SLIDER
  wxT("SLIDER"),
#endif
#ifdef wxUSE_SPINBTN
  wxT("SPINBTN"),
#endif
#ifdef wxUSE_SPINCTRL
  wxT("SPINCTRL"),
#endif
#ifdef wxUSE_SPLITTER
  wxT("SPLITTER"),
#endif
#ifdef wxUSE_STATBMP
  wxT("STATBMP"),
#endif
#ifdef wxUSE_STATBOX
  wxT("STATBOX"),
#endif
#ifdef wxUSE_STATLINE
  wxT("STATLINE"),
#endif
#ifdef wxUSE_STATTEXT
  wxT("STATTEXT"),
#endif
#ifdef wxUSE_STATUSBAR
  wxT("STATUSBAR"),
#endif
#ifdef wxUSE_TOGGLEBTN
  wxT("TOGGLEBTN"),
#endif
#ifdef wxUSE_TAB_DIALOG
  wxT("TAB_DIALOG"),
#endif
#ifdef wxUSE_TABDIALOG
  wxT("TABDIALOG"),
#endif
#ifdef wxUSE_TEXTCTRL
  wxT("TEXTCTRL"),
#endif
#ifdef wxUSE_TOOLBAR
  wxT("TOOLBAR"),
#endif
#ifdef wxUSE_TOOLBAR_NATIVE
  wxT("TOOLBAR_NATIVE"),
#endif
#ifdef wxUSE_TOOLBAR_SIMPLE
  wxT("TOOLBAR_SIMPLE"),
#endif
#ifdef wxUSE_BUTTONBAR
  wxT("BUTTONBAR"),
#endif
#ifdef wxUSE_TREELAYOUT
  wxT("TREELAYOUT"),
#endif
#ifdef wxUSE_TREECTRL
  wxT("TREECTRL"),
#endif
#ifdef wxUSE_LONGLONG
  wxT("LONGLONG"),
#endif
#ifdef wxUSE_GEOMETRY
  wxT("GEOMETRY"),
#endif
#ifdef wxUSE_CMDLINE_PARSER
  wxT("CMDLINE_PARSER"),
#endif
#ifdef wxUSE_DATETIME
  wxT("DATETIME"),
#endif
#ifdef wxUSE_FILE
  wxT("FILE"),
#endif
#ifdef wxUSE_FFILE
  wxT("FFILE"),
#endif
#ifdef wxUSE_FSVOLUME
  wxT("FSVOLUME"),
#endif
#ifdef wxUSE_TEXTBUFFER
  wxT("TEXTBUFFER"),
#endif
#ifdef wxUSE_TEXTFILE
  wxT("TEXTFILE"),
#endif
#ifdef wxUSE_LOG
  wxT("LOG"),
#endif
#ifdef wxUSE_LOGWINDOW
  wxT("LOGWINDOW"),
#endif
#ifdef wxUSE_LOGGUI
  wxT("LOGGUI"),
#endif
#ifdef wxUSE_LOG_DIALOG
  wxT("LOG_DIALOG"),
#endif
#ifdef wxUSE_STOPWATCH
  wxT("STOPWATCH"),
#endif
#ifdef wxUSE_TIMEDATE
  wxT("TIMEDATE"),
#endif
#ifdef wxUSE_WAVE
  wxT("WAVE"),
#endif
#ifdef wxUSE_SOUND
  wxT("SOUND"),
#endif
#ifdef wxUSE_CONFIG
  wxT("CONFIG"),
#endif
#ifdef wxUSE_FONTMAP
  wxT("FONTMAP"),
#endif
#ifdef wxUSE_INTL
  wxT("INTL"),
#endif
#ifdef wxUSE_PROTOCOL
  wxT("PROTOCOL"),
#endif
#ifdef wxUSE_PROTOCOL_FILE
  wxT("PROTOCOL_FILE"),
#endif
#ifdef wxUSE_PROTOCOL_FTP
  wxT("PROTOCOL_FTP"),
#endif
#ifdef wxUSE_PROTOCOL_HTTP
  wxT("PROTOCOL_HTTP"),
#endif
#ifdef wxUSE_STREAMS
  wxT("STREAMS"),
#endif
#ifdef wxUSE_SOCKETS
  wxT("SOCKETS"),
#endif
#ifdef wxUSE_DIALUP_MANAGER
  wxT("DIALUP_MANAGER"),
#endif
#ifdef wxUSE_STD_IOSTREAM
  wxT("STD_IOSTREAM"),
#endif
#ifdef wxUSE_DYNLIB_CLASS
  wxT("DYNLIB_CLASS"),
#endif
#ifdef wxUSE_DYNAMIC_LOADER
  wxT("DYNAMIC_LOADER"),
#endif
#ifdef wxUSE_TIMER
  wxT("TIMER"),
#endif
#ifdef wxUSE_AFM_FOR_POSTSCRIPT
  wxT("AFM_FOR_POSTSCRIPT"),
#endif
#ifdef wxUSE_NORMALIZED_PS_FONTS
  wxT("NORMALIZED_PS_FONTS"),
#endif
#ifdef wxUSE_POSTSCRIPT
  wxT("POSTSCRIPT"),
#endif
#ifdef wxUSE_WCHAR_T
  wxT("WCHAR_T"),
#endif
#ifdef wxUSE_UNICODE
  wxT("UNICODE"),
#endif
#ifdef wxUSE_UNICODE_MSLU
  wxT("UNICODE_MSLU"),
#endif
#ifdef wxUSE_URL
  wxT("URL"),
#endif
#ifdef wxUSE_WCSRTOMBS
  wxT("WCSRTOMBS"),
#endif
#ifdef wxUSE_EXPERIMENTAL_
  wxT("EXPERIMENTAL_PRINTF"),
#endif
#ifdef wxUSE_IPC
  wxT("IPC"),
#endif
#ifdef wxUSE_X_RESOURCES
  wxT("X_RESOURCES"),
#endif
#ifdef wxUSE_CLIPBOARD
  wxT("CLIPBOARD"),
#endif
#ifdef wxUSE_DATAOBJ
  wxT("DATAOBJ"),
#endif
#ifdef wxUSE_TOOLTIPS
  wxT("TOOLTIPS"),
#endif
#ifdef wxUSE_DRAG_AND_DROP
  wxT("DRAG_AND_DROP"),
#endif
#ifdef wxUSE_OLE
  wxT("OLE"),
#endif
#ifdef wxUSE_SPLINES
  wxT("SPLINES"),
#endif
#ifdef wxUSE_MDI_ARCHITECTURE
  wxT("MDI_ARCHITECTURE"),
#endif
#ifdef wxUSE_DOC_VIEW_ARCHITECTURE
  wxT("DOC_VIEW_ARCHITECTURE"),
#endif
#ifdef wxUSE_PRINTING_ARCHITECTURE
  wxT("PRINTING_ARCHITECTURE"),
#endif
#ifdef wxUSE_PROLOGIO
  wxT("PROLOGIO"),
#endif
#ifdef wxUSE_RESOURCES
  wxT("RESOURCES"),
#endif
#ifdef wxUSE_WX_RESOURCES
  wxT("WX_RESOURCES"),
#endif
#ifdef wxUSE_HELP
  wxT("HELP"),
#endif
#ifdef wxUSE_WXHTML_HELP
  wxT("WXHTML_HELP"),
#endif
#ifdef wxUSE_MS_HTML_HELP
  wxT("MS_HTML_HELP"),
#endif
#ifdef wxUSE_IOSTREAMH
  wxT("IOSTREAMH"),
#endif
#ifdef wxUSE_APPLE_IEEE
  wxT("APPLE_IEEE"),
#endif
#ifdef wxUSE_MEMORY_TRACING
  wxT("MEMORY_TRACING"),
#endif
#ifdef wxUSE_DEBUG_NEW_ALWAYS
  wxT("DEBUG_NEW_ALWAYS"),
#endif
#ifdef wxUSE_DEBUG_CONTEXT
  wxT("DEBUG_CONTEXT"),
#endif
#ifdef wxUSE_GLOBAL_MEMORY_OPERATORS
  wxT("GLOBAL_MEMORY_OPERATORS"),
#endif
#ifdef wxUSE_SPLINES
  wxT("SPLINES"),
#endif
#ifdef wxUSE_DYNAMIC_CLASSES
  wxT("DYNAMIC_CLASSES"),
#endif
#ifdef wxUSE_METAFILE
  wxT("METAFILE"),
#endif
#ifdef wxUSE_ENH_METAFILE
  wxT("ENH_METAFILE"),
#endif
#ifdef wxUSE_MINIFRAME
  wxT("MINIFRAME"),
#endif
#ifdef wxUSE_HTML
  wxT("HTML"),
#endif
#ifdef wxUSE_FILESYSTEM
  wxT("FILESYSTEM"),
#endif
#ifdef wxUSE_FS_INET
  wxT("FS_INET"),
#endif
#ifdef wxUSE_FS_ZIP
  wxT("FS_ZIP"),
#endif
#ifdef wxUSE_BUSYINFO
  wxT("BUSYINFO"),
#endif
#ifdef wxUSE_ZIPSTREAM
  wxT("ZIPSTREAM"),
#endif
#ifdef wxUSE_PALETTE
  wxT("PALETTE"),
#endif
#ifdef wxUSE_IMAGE
  wxT("IMAGE"),
#endif
#ifdef wxUSE_GIF
  wxT("GIF"),
#endif
#ifdef wxUSE_PCX
  wxT("PCX"),
#endif
#ifdef wxUSE_IFF
  wxT("IFF"),
#endif
#ifdef wxUSE_PNM
  wxT("PNM"),
#endif
#ifdef wxUSE_XPM
  wxT("XPM"),
#endif
#ifdef wxUSE_ICO_CUR
  wxT("ICO_CUR"),
#endif

// detect using optional libraries in the contrib hierarchy.
#ifdef wxUSE_STC
  wxT("STC"),
#endif
  NULL
};


static const wxChar* hasDefs[] = {
#ifdef wxHAS_POWER_EVENTS
  wxT("POWER_EVENTS"),
#endif
#ifdef wxHAS_RADIO_MENU_ITEMS
  wxT("RADIO_MENU_ITEMS"),
#endif
  NULL
};

/*-----------------------------------------------------------------------------
  EXTERN C
-----------------------------------------------------------------------------*/
extern "C"
{

/*-----------------------------------------------------------------------------
  pre-processor
-----------------------------------------------------------------------------*/
EWXWEXPORT(int, wxVersionNumber)()
{
  return wxVERSION_NUMBER;
}

EWXWEXPORT(int, wxIsDefined)( wxChar* s )
{
  int i;
  if (s==NULL) return 0;
  /* check define */
  for( i=0; defineDefs[i] != NULL; i++ ) {
    if (wxStrcmp(s,defineDefs[i]) == 0) return 1;
  }
  /* check wxUSE_XXX */
  if (wxStrncmp(s,wxT("wxUSE_"),6) == 0) {
    const wxChar* t = s+6;
    for( i=0; useDefs[i] != NULL; i++ ) {
      if (wxStrcmp(t,useDefs[i]) == 0) return 1;
    }
  }
  /* check wxHAS_XXX */
  if (wxStrncmp(s,wxT("wxHAS_"),6) == 0) {
    const wxChar* t = s+6;
    for( i=0; hasDefs[i] != NULL; i++ ) {
      if (wxStrcmp(t,hasDefs[i]) == 0) return 1;
    }
  }
  return 0;
}

EWXWEXPORT(void*, wxcMalloc)(int size )
{
  return malloc(size);
}

EWXWEXPORT(void, wxcFree)( void* p )
{
  if (p!=NULL) free(p);
}

EWXWEXPORT(wxColour*, wxcSystemSettingsGetColour)( int systemColour )
{
   wxColour* colour = new wxColour();
   *colour = wxSystemSettings::GetColour( (wxSystemColour)systemColour );
   return colour;
}

EWXWEXPORT( void, wxcWakeUpIdle)(void)
{
  wxWakeUpIdle();
}

/*-----------------------------------------------------------------------------
  delete
-----------------------------------------------------------------------------*/
EWXWEXPORT(void, wxCursor_Delete) (void* _obj)
{
  delete ((wxCursor*)_obj);
}

EWXWEXPORT(void, wxDateTime_Delete) (void* _obj)
{
  delete ((wxDateTime*)_obj);
}


/*-----------------------------------------------------------------------------
  frame
-----------------------------------------------------------------------------*/
EWXWEXPORT(int, wxFrame_GetTitle) (void* _obj, void* _buf)
{
  wxString result = ((wxFrame*)_obj)->GetTitle();
  return copyStrToBuf(_buf, result);
}

EWXWEXPORT(void, wxFrame_SetTitle) (void* _obj, wxChar* _txt)
{
  ((wxFrame*)_obj)->SetTitle(_txt);
}

EWXWEXPORT(bool, wxFrame_SetShape)( wxFrame* self, wxRegion* region)
{
  return self->SetShape( *region );
}

EWXWEXPORT(bool, wxFrame_ShowFullScreen)( wxFrame* self, bool show, int style)
{
  return self->ShowFullScreen( show, style );
}

EWXWEXPORT(bool, wxFrame_IsFullScreen)( wxFrame* self )
{
  return self->IsFullScreen();
}

EWXWEXPORT(void, wxFrame_Centre)( wxFrame* self, int orientation )
{
  self->Centre();
}


EWXWEXPORT(void, wxNotebook_AssignImageList)( wxNotebook* _obj, wxImageList* imageList )
{
  _obj->AssignImageList(imageList);
}

/*-----------------------------------------------------------------------------
  menu & toolbar
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxMenuBar*,wxMenu_GetMenuBar)(wxMenu* _obj)
{
  return _obj->GetMenuBar();
}


EWXWEXPORT(wxFrame*,wxMenuBar_GetFrame)(wxMenuBar* _obj)
{
  return _obj->GetFrame();
}

EWXWEXPORT(void,wxToolBar_AddTool2)( wxToolBar* _obj, int toolId, wxChar* label, wxBitmap* bmp, wxBitmap* bmpDisabled, int itemKind, wxChar* shortHelp, wxChar* longHelp )
{
  _obj->AddTool(toolId,label,*bmp,*bmpDisabled,(wxItemKind)itemKind,shortHelp,longHelp,NULL);
}

/*-----------------------------------------------------------------------------
  listctrl
-----------------------------------------------------------------------------*/
EWXWEXPORT(int, wxListEvent_GetCacheFrom)(wxListEvent* _obj)
{
  return _obj->GetCacheFrom();
}

EWXWEXPORT(int, wxListEvent_GetCacheTo)(wxListEvent* _obj)
{
  return _obj->GetCacheTo();
}


EWXWEXPORT(void, wxListCtrl_AssignImageList)(wxListCtrl* _obj, wxImageList* images, int which )
{
  _obj->AssignImageList(images,which);
}



EWXWEXPORT(void, wxListCtrl_GetColumn2)(wxListCtrl* _obj, int col, wxListItem* item)
{
  bool success = _obj->GetColumn(col, *item);
  if (!success) item->SetId(-1);
}

EWXWEXPORT(void, wxListCtrl_GetItem2)(wxListCtrl* _obj, wxListItem* info)
{
  bool success = _obj->GetItem(*info);
  if (!success) info->SetId(-1);
}

EWXWEXPORT(void, wxListCtrl_GetItemPosition2)(wxListCtrl* _obj, int item, int* x, int* y)
{
  wxPoint pos;
  bool success = _obj->GetItemPosition((long)item, pos);
  if (success) {
    *x = pos.x;
    *y = pos.y;
  }
  else {
    *x = -1;
    *y = -1;
  }
}


struct SortData {
  long id;
  wxClosure* closure;
};

int wxCALLBACK sortCallBack( long item1, long item2, long data )
{
  wxClosure* closure = ((SortData*)data)->closure;
  long       id      = ((SortData*)data)->id;

  wxCommandEvent event( wxEVT_SORT, id );
  event.SetInt(item1);
  event.SetExtraLong(item2);
  closure->Invoke(&event);
  return event.GetInt();
}

EWXWEXPORT(int, wxListCtrl_SortItems2)(wxListCtrl* _obj, wxClosure* closure )
{
  SortData sortData = { _obj->GetId(), closure };
  return (int)_obj->SortItems( sortCallBack, (long)&sortData );
}



/*-----------------------------------------------------------------------------
  DC
-----------------------------------------------------------------------------*/
EWXWEXPORT(void, wxDC_GetPixel2)(wxDC* _obj, int x, int y, wxColour* col)
{
  bool success = _obj->GetPixel((wxCoord)x, (wxCoord)y, col);
  if (!success) *col = wxNullColour;
}


/*-----------------------------------------------------------------------------
  Object & static ClassInfo
-----------------------------------------------------------------------------*/
EWXWEXPORT(int,wxObject_IsKindOf)(void* _obj, void* classInfo )
{
  return (int)(((wxObject*)_obj)->IsKindOf((wxClassInfo*)classInfo) );
}

EWXWEXPORT(void*,wxObject_GetClassInfo)(void* _obj )
{
  return ((wxObject*)_obj)->GetClassInfo();
}

/* optimize */
EWXWEXPORT(int,wxObject_IsScrolledWindow)(void* _obj)
{
  return (int)(((wxObject*)_obj)->IsKindOf(CLASSINFO(wxScrolledWindow)));
}

EWXWEXPORT(void,wxObject_Delete)(wxObject* _obj)
{
  delete _obj;
}

/*-----------------------------------------------------------------------------
  String
-----------------------------------------------------------------------------*/
typedef char utf8char;

EWXWEXPORT(wxString*,wxString_Create)(wxChar* buffer)
{
  return new wxString(buffer);
}

EWXWEXPORT(wxString*,wxString_CreateUTF8)(utf8char* buffer)
{
  return new wxString (buffer,wxConvUTF8);
}

EWXWEXPORT(wxString*,wxString_CreateLen)(wxChar* buffer,int len)
{
  return new wxString(buffer,len);
}

EWXWEXPORT(void,wxString_Delete)(wxString* s)
{
  delete s;
}

EWXWEXPORT(int,wxString_GetString)(wxString* s,wxChar* buffer)
{
  if (buffer) memcpy (buffer, s->c_str(), s->Length() * sizeof(wxChar));
  return s->Length();
}

EWXWEXPORT(size_t,wxString_Length)(wxString* s)
{
  return s->length();
}

EWXWEXPORT(wxCharBuffer*,wxString_GetUtf8)(wxString* s)
{
  wxCharBuffer *cb = new wxCharBuffer;
  *cb = s->utf8_str();
  return cb;
}

EWXWEXPORT(utf8char*,wxCharBuffer_DataUtf8)(wxCharBuffer* cb)
{
  return (utf8char*)cb->data();
}

EWXWEXPORT(void,wxCharBuffer_Delete)(wxCharBuffer* cb)
{
  delete cb;
}

/*-----------------------------------------------------------------------------
  classinfo
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,wxClassInfo_FindClass)(wxChar* _txt)
{
  return wxClassInfo::FindClass(_txt);
}

EWXWEXPORT(int,wxClassInfo_GetClassNameEx)(void* _obj, void* _buf)
{
  wxString result = ((wxClassInfo*)_obj)->GetClassName();
  return copyStrToBuf(_buf, result);
}

EWXWEXPORT(int,wxClassInfo_GetBaseClassName1)(void* _obj, void* _buf)
{
  wxString result = ((wxClassInfo*)_obj)->GetBaseClassName1();
  return copyStrToBuf(_buf, result);
}

EWXWEXPORT(int,wxClassInfo_GetBaseClassName2)(void* _obj, void* _buf)
{
  wxString result = ((wxClassInfo*)_obj)->GetBaseClassName2();
  return copyStrToBuf(_buf, result);
}

EWXWEXPORT(int,wxClassInfo_IsKindOfEx)(void* _obj, void* classInfo)
{
  return (int)((wxClassInfo*)_obj)->IsKindOf((wxClassInfo*)classInfo);
}

EWXWEXPORT(int,wxClassInfo_GetSize)(void* _obj)
{
  return ((wxClassInfo*)_obj)->GetSize();
}

/*-----------------------------------------------------------------------------
  window
-----------------------------------------------------------------------------*/
EWXWEXPORT(void, wxWindow_ConvertPixelsToDialogEx)(void* _obj, int x, int y, int* _x, int* _y)
{
    wxPoint pt = ((wxWindow*)_obj)->ConvertPixelsToDialog(wxPoint(x, y));
    *_x = pt.x;
    *_y = pt.y;
}

EWXWEXPORT(void, wxWindow_ConvertDialogToPixelsEx)(void* _obj, int x, int y, int* _x, int* _y)
{
    wxPoint pt = ((wxWindow*)_obj)->ConvertDialogToPixels(wxPoint(x, y));
    *_x = pt.x;
    *_y = pt.y;
}


EWXWEXPORT(void, wxWindow_SetClientObject)(void* _obj, void * obj )
{
    ((wxWindow*)_obj)->SetClientObject( (wxClientData*)obj );
}


EWXWEXPORT(void, wxWindow_ScreenToClient2)(wxWindow* self, int x, int y, int *cx, int *cy)
{
  wxPoint pt = self->ScreenToClient( wxPoint(x, y) );
  if (cx) *cx = pt.x;
  if (cy) *cy = pt.y; 
}



EWXWEXPORT(void, wxcGetMousePosition)( int* x, int* y )
{
  wxPoint pt = wxGetMousePosition();
  if (x) *x = pt.x;
  if (y) *y = pt.y;
}

/*-----------------------------------------------------------------------------
  scrolledwindow
-----------------------------------------------------------------------------*/
EWXWEXPORT(void, wxScrolledWindow_SetScrollRate)( wxScrolledWindow* _obj, int xstep, int ystep )
{
  _obj->SetScrollRate(xstep,ystep);
}

/*-----------------------------------------------------------------------------
  mouse
-----------------------------------------------------------------------------*/
EWXWEXPORT(int, wxMouseEvent_GetWheelDelta) (void* _obj)
{
  return ((wxMouseEvent*)_obj)->GetWheelDelta();
}

EWXWEXPORT(int, wxMouseEvent_GetWheelRotation) (void* _obj)
{
  return ((wxMouseEvent*)_obj)->GetWheelRotation();
}

EWXWEXPORT(int, wxMouseEvent_GetButton) (void* _obj)
{
  return ((wxMouseEvent*)_obj)->GetButton();
}

EWXWEXPORT(int,expEVT_MOUSEWHEEL)()
{
    return (int)wxEVT_MOUSEWHEEL;
}


/*-----------------------------------------------------------------------------
  DC
-----------------------------------------------------------------------------*/
EWXWEXPORT(double, wxDC_GetUserScaleX)( wxDC* dc )
{
  double x = 1.0;
  double y = 1.0;
  dc->GetUserScale(&x,&y);
  return x;
}


EWXWEXPORT(double, wxDC_GetUserScaleY)( wxDC* dc )
{
  double x = 1.0;
  double y = 1.0;
  dc->GetUserScale(&x,&y);
  return y;
}


/*-----------------------------------------------------------------------------
  timers
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*, wxTimerEx_Create)()
{
  return new wxTimerEx();
}

EWXWEXPORT(void, wxTimerEx_Connect)(void* _obj, void* _closure )
{
  ((wxTimerEx*)_obj)->Connect((wxClosure*)_closure);
}

EWXWEXPORT(void*, wxTimerEx_GetClosure)(void* _obj)
{
  return (void*)(((wxTimerEx*)_obj)->GetClosure());
}


/*-----------------------------------------------------------------------------
  menu items
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*, wxMenuItem_CreateSeparator)()
{
  return (void*) new wxMenuItem( NULL, wxID_SEPARATOR, wxT(""), wxT(""), wxITEM_SEPARATOR, NULL );
}


EWXWEXPORT(void*, wxMenuItem_CreateEx)(int id, wxChar* text, wxChar* helpstr, int itemkind, void* submenu)
{
  return (void*) new wxMenuItem( NULL, id, text, helpstr, (wxItemKind)itemkind, (wxMenu*)submenu );
}


EWXWEXPORT(void, wxMenu_AppendRadioItem)(wxMenu* self, int id, wxChar* text, wxChar* help)
{
#ifdef wxHAS_RADIO_MENU_ITEMS
  self->AppendRadioItem(id, text, help);
#else
  self->AppendCheckItem(id, text, help);
#endif
}


/*------------------------------------------------------------------------------
  process
------------------------------------------------------------------------------*/
EWXWEXPORT(int, wxProcess_IsErrorAvailable)(void* _obj)
{
    return ((wxProcess*)_obj)->IsErrorAvailable();
}

EWXWEXPORT(int, wxProcess_IsInputAvailable)(void* _obj)
{
    return ((wxProcess*)_obj)->IsInputAvailable();
}

EWXWEXPORT(int, wxProcess_IsInputOpened)(void* _obj)
{
    return ((wxProcess*)_obj)->IsInputOpened();
}

EWXWEXPORT(wxProcess*, wxProcess_Open)( wxChar* cmd, int flags )
{
    return wxProcess::Open( cmd, ((flags | wxEXEC_ASYNC) & ~wxEXEC_SYNC) );
}

EWXWEXPORT(wxKillError, wxKill)( int pid, wxSignal signal )
{
  return wxProcess::Kill(pid,signal);
}

EWXWEXPORT(void,wxStreamBase_Delete)(wxStreamBase* stream)
{
  if (stream) delete stream;
}

/*------------------------------------------------------------------------------
  TextCtrl
------------------------------------------------------------------------------*/
EWXWEXPORT(int, wxTextCtrl_EmulateKeyPress)(void * _obj, void *keyevent)
{
    return ((wxTextCtrl*)_obj)->EmulateKeyPress( * ((wxKeyEvent *) keyevent));
}


EWXWEXPORT( void *, wxTextCtrl_GetDefaultStyle)(void * _obj)
{
    return (void *) & ((wxTextCtrl*)_obj)->GetDefaultStyle();
}

EWXWEXPORT( int, wxTextCtrl_GetRange)(void * _obj, long from, long to, void *_buf)
{
    wxString result = ((wxTextCtrl*)_obj)->GetRange(from, to);
    return copyStrToBuf(_buf, result);
}

EWXWEXPORT( int, wxTextCtrl_GetStringSelection)(void * _obj, void *_buf)
{
    wxString result = ((wxTextCtrl*)_obj)->GetStringSelection();
    return copyStrToBuf(_buf, result);
}

EWXWEXPORT( int, wxTextCtrl_IsMultiLine)(void * _obj)
{
    return (int) ((wxTextCtrl*)_obj)->IsMultiLine();
}

EWXWEXPORT( int, wxTextCtrl_IsSingleLine)(void * _obj)
{
    return (int) ((wxTextCtrl*)_obj)->IsSingleLine(   );
}


EWXWEXPORT(int, wxTextCtrl_SetDefaultStyle)(void * _obj, void *style)
{
    return (int) ((wxTextCtrl*)_obj)->SetDefaultStyle( * (wxTextAttr *) style  );
}

EWXWEXPORT(void, wxTextCtrl_SetMaxLength)(void * _obj, long len)
{
    ((wxTextCtrl*)_obj)->SetMaxLength( len  );
}

EWXWEXPORT(int, wxTextCtrl_SetStyle)(void * _obj, long start, long end, void * style)
{
    return (int) ((wxTextCtrl*)_obj)->SetStyle(start, end, * (wxTextAttr *) style);
}

/*------------------------------------------------------------------------------
  TextAttr
------------------------------------------------------------------------------*/
EWXWEXPORT( wxTextAttr*, wxTextAttr_CreateDefault)()
{
  return new wxTextAttr();
}

EWXWEXPORT( void *, wxTextAttr_Create)(void * colText, void * colBack, void *font)
{
    return (void *) new wxTextAttr( * (wxColour *) colText, * (wxColour *) colBack, * (wxFont *) font );
}

EWXWEXPORT( void, wxTextAttr_Delete)(void * _obj)
{
    delete ((wxTextAttr*)_obj);
}

EWXWEXPORT(void, wxTextAttr_GetBackgroundColour)(wxTextAttr* _obj, wxColour* colour )
{
    *colour = _obj->GetBackgroundColour();
}

EWXWEXPORT(void, wxTextAttr_GetFont)(wxTextAttr* _obj, wxFont* font )
{
    *font = _obj->GetFont();
}

EWXWEXPORT(void, wxTextAttr_GetTextColour)(wxTextAttr* _obj, wxColour* colour )
{
    *colour = _obj->GetTextColour();
}

EWXWEXPORT(int, wxTextAttr_HasBackgroundColour)(void * _obj)
{
    return (int) ((wxTextAttr*)_obj)->HasBackgroundColour();
}

EWXWEXPORT(int, wxTextAttr_HasFont)(void * _obj)
{
    return (int) ((wxTextAttr*)_obj)->HasFont();
}

EWXWEXPORT(int, wxTextAttr_HasTextColour)(void * _obj)
{
    return (int) ((wxTextAttr*)_obj)->HasTextColour(   );
}

EWXWEXPORT(int, wxTextAttr_IsDefault)(void * _obj)
{
    return (int) ((wxTextAttr*)_obj)->IsDefault(   );
}

EWXWEXPORT( void, wxTextAttr_SetTextColour)(wxTextAttr* _obj, wxColour* colour )
{
   _obj->SetTextColour(*colour);
}

EWXWEXPORT( void, wxTextAttr_SetBackgroundColour)(wxTextAttr* _obj, wxColour* colour )
{
   _obj->SetBackgroundColour(*colour);
}

EWXWEXPORT( void, wxTextAttr_SetFont)(wxTextAttr* _obj, wxFont* font )
{
   _obj->SetFont(*font);
}

/*------------------------------------------------------------------------------
  progress dialog
------------------------------------------------------------------------------*/
EWXWEXPORT(wxProgressDialog*, wxProgressDialog_Create)( wxChar* title, wxChar* message, int max, wxWindow* parent, int style )
{
  return new wxProgressDialog( title, message, max, parent, style );
}

EWXWEXPORT(int, wxProgressDialog_Update)(wxProgressDialog* obj, int value )
{
  return (obj->Update(value) ? 1 : 0);
}

EWXWEXPORT(int, wxProgressDialog_UpdateWithMessage)(wxProgressDialog* obj, int value, wxChar* message )
{
  return (obj->Update(value,message) ? 1 : 0);
}

EWXWEXPORT(void, wxProgressDialog_Resume)(wxProgressDialog* obj )
{
  obj->Resume();
}


/*------------------------------------------------------------------------------
  standard dialogs
------------------------------------------------------------------------------*/
EWXWEXPORT(void, wxGetColourFromUser)(wxWindow *parent, wxColour* colInit, wxColour* colour)
{
  *colour = wxGetColourFromUser(parent, *colInit);
}

EWXWEXPORT(void, wxGetFontFromUser)(wxWindow *parent, wxFont* fontInit, wxFont* font )
{
  *font = wxGetFontFromUser(parent, *fontInit);
}

EWXWEXPORT(int, wxGetPasswordFromUser)(wxChar* message, wxChar* caption, wxChar* defaultText, wxWindow* parent, wxChar* _buf )
{
/* we use a complicated caching method as we don't want to call getpassword twice :-) */
  static wxChar* resultBuffer = NULL;
  if (_buf==NULL) {
    if (resultBuffer) { free(resultBuffer); resultBuffer = NULL; }
    wxString result = wxGetPasswordFromUser( message, caption, defaultText, parent );
    resultBuffer = (wxChar*)malloc( (result.Length() + 1) * sizeof(wxChar) );
    if (resultBuffer) {
      wxStrcpy( resultBuffer, result.c_str() ); /* save result */
      return result.Length();
    }
    else {
      return 0;
    }
  }
  else if (resultBuffer) {
    int len = wxStrlen(resultBuffer);
    memcpy(_buf, resultBuffer, len * sizeof(wxChar) );  /* copy saved result */
    free(resultBuffer);
    resultBuffer = NULL;
    return len;
  }
  else {
    return 0;
  }
}

EWXWEXPORT(int, wxGetTextFromUser)(wxChar* message, wxChar* caption, wxChar* defaultText, wxWindow* parent, int x, int y, int center, wxChar* _buf )
{
/* we use a complicated caching method as we don't want to call gettext twice :-) */
  static wxChar* resultBuffer = NULL;
  if (_buf==NULL) {
    if (resultBuffer) { free(resultBuffer); resultBuffer = NULL; }
    wxString result = wxGetTextFromUser( message, caption, defaultText, parent, x, y, center!=0 );
    resultBuffer = (wxChar*)malloc( (result.Length() + 1) * sizeof(wxChar) );
    if (resultBuffer) {
      wxStrcpy( resultBuffer, result.c_str() ); /* save result */
      return result.Length();
    }
    else {
      return 0;
    }
  }
  else if (resultBuffer) {
    int len = wxStrlen(resultBuffer);
    memcpy(_buf, resultBuffer, len * sizeof(wxChar) );  /* copy saved result */
    free(resultBuffer);
    resultBuffer = NULL;
    return len;
  }
  else {
    return 0;
  }
}

EWXWEXPORT(long,wxGetNumberFromUser)( wxChar* message, wxChar* prompt, wxChar* caption, long value, long min, long max, wxWindow* parent, int x, int y )
{
  return wxGetNumberFromUser(wxString(message), wxString(prompt), wxString(caption), value, min, max, parent, wxPoint(x, y) );
}

EWXWEXPORT(void, wxcBell)(void)
{
  wxBell();
}

EWXWEXPORT(void, wxcBeginBusyCursor)(void)
{
  wxBeginBusyCursor();
}

EWXWEXPORT(int, wxcIsBusy)(void)
{
  return (wxIsBusy() != 0);
}

EWXWEXPORT(void, wxcEndBusyCursor)(void)
{
  wxEndBusyCursor();
}

/*-----------------------------------------------------------------------------
  wxInputSink
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxInputSink*, wxInputSink_Create)( wxInputStream* input, wxEvtHandler* evtHandler, int bufferLen )
{
  return new wxInputSink(input,evtHandler,bufferLen);
}

EWXWEXPORT(int, wxInputSink_GetId)( wxInputSink* obj )
{
  return obj->GetId();
}

EWXWEXPORT(void, wxInputSink_Start)( wxInputSink* obj )
{
  obj->Start();
}


EWXWEXPORT(int, wxInputSinkEvent_LastError)( wxInputSinkEvent* obj )
{
  return obj->LastError();
}

EWXWEXPORT(int, wxInputSinkEvent_LastRead)( wxInputSinkEvent* obj )
{
  return obj->LastRead();
}

EWXWEXPORT(char*, wxInputSinkEvent_LastInput)( wxInputSinkEvent* obj )
{
  return obj->LastInput();
}


/*-----------------------------------------------------------------------------
  html window
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,wxcHtmlEvent_GetMouseEvent)( void* _obj )
{
    return (void*)(((wxcHtmlEvent*)_obj)->GetMouseEvent());
}

EWXWEXPORT(void*,wxcHtmlEvent_GetHtmlCell)( void* _obj  )
{
    return (void*)(((wxcHtmlEvent*)_obj)->GetHtmlCell());
}

EWXWEXPORT(wxString*,wxcHtmlEvent_GetHtmlCellId)( wxcHtmlEvent* _obj)
{
    return _obj->GetHtmlCellId();
}

EWXWEXPORT(wxString*,wxcHtmlEvent_GetHref)( wxcHtmlEvent* _obj)
{
    return _obj->GetHref();
}

EWXWEXPORT(wxString*,wxcHtmlEvent_GetTarget)( wxcHtmlEvent* _obj )
{
    return _obj->GetTarget();
}

EWXWEXPORT(void,wxcHtmlEvent_GetLogicalPosition)( wxcHtmlEvent* _obj, int* x, int* y )
{
    wxPoint p = _obj->GetLogicalPosition();
    if (x) *x = p.x;
    if (y) *y = p.y;
    return;
}


/*-----------------------------------------------------------------------------
  html window
-----------------------------------------------------------------------------*/
EWXWEXPORT( void*, wxHtmlWindow_Create)(void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, long _stl, wxChar* _txt)
{
    return (void *) (wxHtmlWindow*) new wxHtmlWindow((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, _txt );
}

EWXWEXPORT( void*, wxcHtmlWindow_Create)(void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, long _stl, wxChar* _txt)
{
    return (void *) (wxcHtmlWindow*) new wxcHtmlWindow((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, _txt );
}


EWXWEXPORT( int, wxHtmlWindow_AppendToPage)(void * _obj, wxChar * source)
{
    return (int) ((wxHtmlWindow*)_obj)->AppendToPage( source  );
}

EWXWEXPORT( void *, wxHtmlWindow_GetInternalRepresentation)(void * _obj)
{
    return (void *) ((wxHtmlWindow*)_obj)->GetInternalRepresentation(   );
}

EWXWEXPORT( int, wxHtmlWindow_GetOpenedAnchor)(void * _obj, void *_buf)
{
    wxString result = ((wxHtmlWindow*)_obj)->GetOpenedAnchor();
    if (_buf) memcpy (_buf, result.c_str(), result.Length());
    return result.Length();
}

EWXWEXPORT( int, wxHtmlWindow_GetOpenedPage)(void * _obj, void *_buf)
{
    wxString result = ((wxHtmlWindow*)_obj)->GetOpenedPage();
    if (_buf) memcpy (_buf, result.c_str(), result.Length());
    return result.Length();
}

EWXWEXPORT( int, wxHtmlWindow_GetOpenedPageTitle)(void * _obj, void *_buf)
{
    wxString result = ((wxHtmlWindow*)_obj)->GetOpenedPageTitle();
    if (_buf) memcpy (_buf, result.c_str(), result.Length());
    return result.Length();
}


EWXWEXPORT( void *, wxHtmlWindow_GetRelatedFrame)(void * _obj)
{
    return (void *) ((wxHtmlWindow*)_obj)->GetRelatedFrame(   );
}

EWXWEXPORT( int, wxHtmlWindow_HistoryBack)(void * _obj)
{
    return (int) ((wxHtmlWindow*)_obj)->HistoryBack(   );
}

EWXWEXPORT( int, wxHtmlWindow_HistoryCanBack)(void * _obj)
{
    return (int) ((wxHtmlWindow*)_obj)->HistoryCanBack(   );
}

EWXWEXPORT( int, wxHtmlWindow_HistoryCanForward)(void * _obj)
{
    return (int) ((wxHtmlWindow*)_obj)->HistoryCanForward(   );
}

EWXWEXPORT( void, wxHtmlWindow_HistoryClear)(void * _obj)
{
    ((wxHtmlWindow*)_obj)->HistoryClear(   );
}

EWXWEXPORT( int, wxHtmlWindow_HistoryForward)(void * _obj)
{
    return (int) ((wxHtmlWindow*)_obj)->HistoryForward(   );
}

EWXWEXPORT( int, wxHtmlWindow_LoadPage)(void * _obj, wxChar *location)
{
    return (int) ((wxHtmlWindow*)_obj)->LoadPage(location );
}


EWXWEXPORT( void, wxHtmlWindow_ReadCustomization)(void * _obj, void * cfg, wxChar * path)
{
    ((wxHtmlWindow*)_obj)->ReadCustomization( ((wxConfigBase *) cfg), path );
}

EWXWEXPORT(  void, wxHtmlWindow_SetBorders)(void * _obj, int b)
{
    ((wxHtmlWindow*)_obj)->SetBorders( b );
}

EWXWEXPORT( void, wxHtmlWindow_SetFonts)(void * _obj, wxChar * normal_face, wxChar * fixed_face, int *sizes)
{
    ((wxHtmlWindow*)_obj)->SetFonts(normal_face, fixed_face, sizes );
}

EWXWEXPORT( int, wxHtmlWindow_SetPage)(void * _obj, wxChar * source)
{
    return ((wxHtmlWindow*)_obj)->SetPage( source );
}

EWXWEXPORT( void, wxHtmlWindow_SetRelatedFrame)(void * _obj, void * frame, wxChar * format)
{
    ((wxHtmlWindow*)_obj)->SetRelatedFrame(  ((wxFrame *) frame), format);
}

EWXWEXPORT( void, wxHtmlWindow_SetRelatedStatusBar)(void * _obj, int bar)
{
    ((wxHtmlWindow*)_obj)->SetRelatedStatusBar(bar);
}

EWXWEXPORT( void, wxHtmlWindow_WriteCustomization)(void * _obj, void *cfg, wxChar * path)
{
    ((wxHtmlWindow*)_obj)->WriteCustomization( ((wxConfigBase *) cfg), path );
}

/*-----------------------------------------------------------------------------
  LOGGER
-----------------------------------------------------------------------------*/
EWXWEXPORT( wxLogStderr*, wxLogStderr_Create)()
{
  return new wxLogStderr();
}

EWXWEXPORT( wxLogStderr*, wxLogStderr_CreateStdOut)()
{
  return new wxLogStderr(stdout);
}

EWXWEXPORT( wxLogNull*, wxLogNull_Create)()
{
  return new wxLogNull();
}

EWXWEXPORT( wxLogTextCtrl*, wxLogTextCtrl_Create)( wxTextCtrl* text )
{
  return new wxLogTextCtrl(text);
}

EWXWEXPORT( wxLogWindow*, wxLogWindow_Create)( wxFrame* parent, wxChar* title, int showit, int passthrough )
{
  return new wxLogWindow(parent,title,showit,passthrough);
}

EWXWEXPORT( wxFrame*, wxLogWindow_GetFrame)( wxLogWindow* obj )
{
  return obj->GetFrame();
}

EWXWEXPORT(void,wxLog_Delete)(void* _obj)
{
        delete (wxLog*)_obj;
}

EWXWEXPORT(void,wxLog_OnLog)(void* _obj, int level, void* szString, int t)
{
        ((wxLog*)_obj)->OnLog((wxLogLevel)level, (const wxChar*)szString, (time_t)t);
}

EWXWEXPORT(void,wxLog_Flush)(void* _obj)
{
        ((wxLog*)_obj)->Flush();
}

EWXWEXPORT(int,wxLog_HasPendingMessages)(void* _obj)
{
        return (int)((wxLog*)_obj)->HasPendingMessages();
}

EWXWEXPORT(void,wxLog_FlushActive)(void* _obj)
{
        ((wxLog*)_obj)->FlushActive();
}

EWXWEXPORT(void*,wxLog_GetActiveTarget)()
{
        return (void*)wxLog::GetActiveTarget();
}

EWXWEXPORT(void*,wxLog_SetActiveTarget)(void* pLogger)
{
        return (void*)wxLog::SetActiveTarget((wxLog*)pLogger);
}

EWXWEXPORT(void,wxLog_Suspend)(void* _obj)
{
        ((wxLog*)_obj)->Suspend();
}

EWXWEXPORT(void,wxLog_Resume)(void* _obj)
{
        ((wxLog*)_obj)->Resume();
}

EWXWEXPORT(void,wxLog_SetVerbose)(void* _obj, int bVerbose)
{
        ((wxLog*)_obj)->SetVerbose(bVerbose != 0);
}

EWXWEXPORT(void,wxLog_DontCreateOnDemand)(void* _obj)
{
        ((wxLog*)_obj)->DontCreateOnDemand();
}

EWXWEXPORT(void,wxLog_SetTraceMask)(void* _obj, int ulMask)
{
        ((wxLog*)_obj)->SetTraceMask((wxTraceMask)ulMask);
}

EWXWEXPORT(void,wxLog_AddTraceMask)(void* _obj, void* str)
{
        ((wxLog*)_obj)->AddTraceMask((const wxChar*)str);
}

EWXWEXPORT(void,wxLog_RemoveTraceMask)(void* _obj, void* str)
{
        ((wxLog*)_obj)->RemoveTraceMask((const wxChar*)str);
}

EWXWEXPORT(void,wxLog_SetTimestamp)(void* _obj, void* ts)
{
        ((wxLog*)_obj)->SetTimestamp((const wxChar*)ts);
}

EWXWEXPORT(int,wxLog_GetVerbose)(void* _obj)
{
        return (int)((wxLog*)_obj)->GetVerbose();
}

EWXWEXPORT(int,wxLog_GetTraceMask)(void* _obj)
{
        return (int)((wxLog*)_obj)->GetTraceMask();
}

EWXWEXPORT(int,wxLog_IsAllowedTraceMask)(void* _obj, void* mask)
{
        return (int)((wxLog*)_obj)->IsAllowedTraceMask((const wxChar*)mask);
}

EWXWEXPORT(void*,wxLog_GetTimestamp)(void* _obj)
{
        return (void*)((wxLog*)_obj)->GetTimestamp();
}


EWXWEXPORT(void,LogError)(void* _msg)
{
        wxLogError((wxChar*)_msg);
}

EWXWEXPORT(void,LogFatalError)(void* _msg)
{
        wxLogFatalError((wxChar*)_msg);
}

EWXWEXPORT(void,LogWarning)(void* _msg)
{
        wxLogWarning((wxChar*)_msg);
}

EWXWEXPORT(void,LogMessage)(void* _msg)
{
        wxLogMessage((wxChar*)_msg);
}

EWXWEXPORT(void,LogVerbose)(void* _msg)
{
        wxLogVerbose((wxChar*)_msg);
}

EWXWEXPORT(void,LogStatus)(void* _msg)
{
        wxLogStatus((wxChar*)_msg);
}

EWXWEXPORT(void,LogSysError)(void* _msg)
{
        wxLogSysError((wxChar*)_msg);
}

EWXWEXPORT(void,LogDebug)(void* _msg)
{
        wxLogDebug((wxChar*)_msg);
}

EWXWEXPORT(void,LogTrace)(void* mask, void* _msg)
{
        wxLogTrace((wxChar*) mask, (wxChar*)_msg);
}

/*-----------------------------------------------------------------------------
  Grid text editor
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxGridCellTextEnterEditor*,wxGridCellTextEnterEditor_Ctor)()
{
  return new wxGridCellTextEnterEditor();
}

/*-----------------------------------------------------------------------------
  ConfigBase
-----------------------------------------------------------------------------*/
EWXWEXPORT( wxConfigBase*, wxConfigBase_Get)()
{
  return wxConfigBase::Get();
}

EWXWEXPORT( void, wxConfigBase_Set)( wxConfigBase* self )
{
  wxConfigBase::Set( self );
}

EWXWEXPORT( wxFileConfig*, wxFileConfig_Create)( wxInputStream* inp )
{
  return new wxFileConfig( *inp );
}


} /* extern "C" */
