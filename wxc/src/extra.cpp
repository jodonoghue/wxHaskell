#include "wrapper.h"
#include "wx/process.h"
#include "wx/textctrl.h"
#include "wx/progdlg.h"
#include "wx/listctrl.h"
#include "wx/grid.h"
#include "wx/fileconf.h"
#include "wx/spinctrl.h"

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

  int  GetId();
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

int wxInputSink::GetId()
{
  return (int)m_input;
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

   void OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event);
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

void wxcHtmlWindow::OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event)
{
    wxHtmlLinkInfo* linkPtr;
    if (cell==NULL) return;

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
static const char* defineDefs[] = {
#ifdef __WINDOWS__
  "__WINDOWS__",
#endif // any Windows, yom may also use
#ifdef __WIN16__
  "__WIN16__",
#endif // Win16 API
#ifdef __WIN32__
  "__WIN32__",
#endif // Win32 API
#ifdef __WIN95__
  "__WIN95__",
#endif // Windows 95 or NT 4.0 and above system (not NT 3.5x)
#ifdef __WXGTK__
  "__WXGTK__",
#endif // GTK
#ifdef __WXGTK12__
  "__WXGTK12__",
#endif // GTK 1.2 or higher
#ifdef __WXGTK20__
  "__WXGTK20__",
#endif // GTK 2.0 or higher
#ifdef __WXMOTIF__
  "__WXMOTIF__",
#endif // Motif
#ifdef __WXMAC__
  "__WXMAC__",
#endif // MacOS
#ifdef __WXMGL__
  "__WXMGL__",
#endif // SciTech Soft MGL
#ifdef __WXUNIVERSAL__
  "__WXUNIVERSAL__",
#endif //will be also defined)
#ifdef __WXMSW__
  "__WXMSW__",
#endif // Any Windows
#ifdef __WXOS2__
  "__WXOS2__",
#endif // OS/2 native Presentation Manager
#ifdef __WXPM__
  "__WXPM__",
#endif // OS/2 native Presentation Manager
#ifdef __WXSTUBS__
  "__WXSTUBS__",
#endif // Stubbed version ('template' wxWin implementation)
#ifdef __WXXT__
  "__WXXT__",
#endif // Xt; mutually exclusive with WX_MOTIF, not implemented in wxWindows 2.x
// wxX11
#ifdef __WXX11__
  "__WXX11__",
#endif
#ifdef __WXUNIVERSAL__
  "__WXUNIVERSAL__",
#endif //will be also defined)
#ifdef __WXWINE__
  "__WXWINE__",
#endif // WINE (i.e. Win32 on Unix)
#ifdef __WXUNIVERSAL__
  "__WXUNIVERSAL__",
#endif // wxUniversal port, always defined in addition to one of the symbols above so this should be tested first.
#ifdef __X__
  "__X__",
#endif // any X11-based GUI toolkit except GTK+

// any Mac OS version
#ifdef __APPLE__
  "__APPLE__",
#endif
// AIX
#ifdef __AIX__
  "__AIX__",
#endif
// Any BSD
#ifdef __BSD__
  "__BSD__",
#endif
// Cygwin: Unix on Win32
#ifdef __CYGWIN__
  "__CYGWIN__",
#endif
// Mac OS X
#ifdef __DARWIN__
  "__DARWIN__",
#endif
#ifdef ____DATA_GENERAL____
  "__DATA_GENERAL__",
#endif
// DOS (used with wxMGL only)
#ifdef __DOS_GENERAL__
  "__DOS_GENERAL__",
#endif
// FreeBSD
#ifdef __FREEBSD__
  "__FREEBSD__",
#endif
// HP-UX (Unix)
#ifdef ____HPUX____
  "__HPUX__",
#endif
// Linux
#ifdef __LINUX__
  "__LINUX__",
#endif
// OSF/1
#ifdef __OSF__
  "__OSF__",
#endif
// IRIX
#ifdef __SGI__
  "__SGI__",
#endif
// Solaris
#ifdef __SOLARIS__
  "__SOLARIS__",
#endif
// Any Sun
#ifdef __SUN__
  "__SUN__",
#endif
// Sun OS
#ifdef __SUNOS__
  "__SUNOS__",
#endif
// SystemV R4
#ifdef __SVR4__
  "__SVR4__",
#endif
// SystemV generic
#ifdef __SYSV__
  "__SYSV__",
#endif
// Ultrix
#ifdef __ULTRIX__
  "__ULTRIX__",
#endif
// any Unix
#ifdef __UNIX__
  "__UNIX__",
#endif
// Unix, BeOS or VMS
#ifdef __UNIX_LIKE__
  "__UNIX_LIKE__",
#endif
// VMS
#ifdef __VMS__
  "__VMS__",
#endif
// any Windows
#ifdef __WINDOWS__
  "__WINDOWS__",
#endif

// DEC Alpha architecture
#ifdef __ALPHA__
  "__ALPHA__",
#endif
// Intel i386 or compatible
#ifdef __INTEL__
  "__INTEL__",
#endif
// Motorola Power PC
#ifdef __POWERPC__
  "__POWERPC__",
#endif

// Borland C++
#ifdef __BORLANDC__
  "__BORLANDC__",
#endif
// DJGPP
#ifdef __DJGPP__
  "__DJGPP__",
#endif
// Gnu C++ on any platform
#ifdef __GNUG__
  "__GNUG__",
#endif
// GnuWin32 compiler
#ifdef __GNUWIN32__
  "__GNUWIN32__",
#endif
// CodeWarrior MetroWerks compiler
#ifdef __MWERKS__
  "__MWERKS__",
#endif
// Sun CC
#ifdef __SUNCC__
  "__SUNCC__",
#endif
// Symantec C++
#ifdef __SYMANTECC__
  "__SYMANTECC__",
#endif
// IBM Visual Age (OS/2)
#ifdef __VISAGECPP__
  "__VISAGECPP__",
#endif
// Microsoft Visual C++
#ifdef __VISUALC__
  "__VISUALC__",
#endif
// AIX compiler
#ifdef __XLC__
  "__XLC__",
#endif
// Watcom C++
#ifdef __WATCOMC__
  "__WATCOMC__",
#endif
  NULL
};

static const char* useDefs[] = {
#ifdef wxUSE_UNIX
  "UNIX",
#endif
#ifdef wxUSE_NANOX
  "NANOX",
#endif
#ifdef wxUSE_NATIVE_STATUSBAR
  "NATIVE_STATUSBAR",
#endif
#ifdef wxUSE_OWNER_DRAWN
  "OWNER_DRAWN",
#endif
#ifdef wxUSE_OWNER_DRAWN
  "OWNER_DRAWN",
#endif
#ifdef wxUSE_RICHEDIT
  "RICHEDIT",
#endif
#ifdef wxUSE_RICHEDIT
  "RICHEDIT",
#endif
#ifdef wxUSE_REGEX
  "REGEX",
#endif
#ifdef wxUSE_ZLIB
  "ZLIB",
#endif
#ifdef wxUSE_LIBPNG
  "LIBPNG",
#endif
#ifdef wxUSE_LIBJPEG
  "LIBJPEG",
#endif
#ifdef wxUSE_LIBTIFF
  "LIBTIFF",
#endif
#ifdef wxUSE_ODBC
  "ODBC",
#endif
#ifdef wxUSE_FREETYPE
  "FREETYPE",
#endif
#ifdef wxUSE_THREADS
  "THREADS",
#endif
#ifdef wxUSE_OPENGL
  "OPENGL",
#endif
#ifdef wxUSE_GLCANVAS
  "GLCANVAS",
#endif
#ifdef wxUSE_GUI
  "GUI",
#endif
#ifdef wxUSE_NOGUI
  "NOGUI",
#endif
#ifdef wxUSE_ON_FATAL_EXCEPTION
  "ON_FATAL_EXCEPTION",
#endif
#ifdef wxUSE_SNGLINST_CHECKER
  "SNGLINST_CHECKER",
#endif
#ifdef wxUSE_CONSTRAINTS
  "CONSTRAINTS",
#endif
#ifdef wxUSE_VALIDATORS
  "VALIDATORS",
#endif
#ifdef wxUSE_CONTROLS
  "CONTROLS",
#endif
#ifdef wxUSE_POPUPWIN
  "POPUPWIN",
#endif
#ifdef wxUSE_TIPWINDOW
  "TIPWINDOW",
#endif
#ifdef wxUSE_ACCEL
  "ACCEL",
#endif
#ifdef wxUSE_CALENDARCTRL
  "CALENDARCTRL",
#endif
#ifdef wxUSE_FILEDLG
  "FILEDLG",
#endif
#ifdef wxUSE_FINDREPLDLG
  "FINDREPLDLG",
#endif
#ifdef wxUSE_FONTDLG
  "FONTDLG",
#endif
#ifdef wxUSE_MIMETYPE
  "MIMETYPE",
#endif
#ifdef wxUSE_SYSTEM_OPTIONS
  "SYSTEM_OPTIONS",
#endif
#ifdef wxUSE_MSGDLG
  "MSGDLG",
#endif
#ifdef wxUSE_NUMBERDLG
  "NUMBERDLG",
#endif
#ifdef wxUSE_TEXTDLG
  "TEXTDLG",
#endif
#ifdef wxUSE_STARTUP_TIPS
  "STARTUP_TIPS",
#endif
#ifdef wxUSE_PROGRESSDLG
  "PROGRESSDLG",
#endif
#ifdef wxUSE_CHOICEDLG
  "CHOICEDLG",
#endif
#ifdef wxUSE_COLOURDLG
  "COLOURDLG",
#endif
#ifdef wxUSE_DIRDLG
  "DIRDLG",
#endif
#ifdef wxUSE_DRAGIMAGE
  "DRAGIMAGE",
#endif
#ifdef wxUSE_PROPSHEET
  "PROPSHEET",
#endif
#ifdef wxUSE_WIZARDDLG
  "WIZARDDLG",
#endif
#ifdef wxUSE_SPLASH
  "SPLASH",
#endif
#ifdef wxUSE_JOYSTICK
  "JOYSTICK",
#endif
#ifdef wxUSE_BUTTON
  "BUTTON",
#endif
#ifdef wxUSE_CARET
  "CARET",
#endif
#ifdef wxUSE_BMPBUTTON
  "BMPBUTTON",
#endif
#ifdef wxUSE_CHECKBOX
  "CHECKBOX",
#endif
#ifdef wxUSE_CHECKLISTBOX
  "CHECKLISTBOX",
#endif
#ifdef wxUSE_COMBOBOX
  "COMBOBOX",
#endif
#ifdef wxUSE_CHOICE
  "CHOICE",
#endif
#ifdef wxUSE_GAUGE
  "GAUGE",
#endif
#ifdef wxUSE_GRID
  "GRID",
#endif
#ifdef wxUSE_NEW_GRID
  "NEW_GRID",
#endif
#ifdef wxUSE_IMAGLIST
  "IMAGLIST",
#endif
#ifdef wxUSE_LISTBOX
  "LISTBOX",
#endif
#ifdef wxUSE_LISTCTRL
  "LISTCTRL",
#endif
#ifdef wxUSE_MENUS
  "MENUS",
#endif
#ifdef wxUSE_NOTEBOOK
  "NOTEBOOK",
#endif
#ifdef wxUSE_RADIOBOX
  "RADIOBOX",
#endif
#ifdef wxUSE_RADIOBTN
  "RADIOBTN",
#endif
#ifdef wxUSE_SASH
  "SASH",
#endif
#ifdef wxUSE_SCROLLBAR
  "SCROLLBAR",
#endif
#ifdef wxUSE_SLIDER
  "SLIDER",
#endif
#ifdef wxUSE_SPINBTN
  "SPINBTN",
#endif
#ifdef wxUSE_SPINCTRL
  "SPINCTRL",
#endif
#ifdef wxUSE_SPLITTER
  "SPLITTER",
#endif
#ifdef wxUSE_STATBMP
  "STATBMP",
#endif
#ifdef wxUSE_STATBOX
  "STATBOX",
#endif
#ifdef wxUSE_STATLINE
  "STATLINE",
#endif
#ifdef wxUSE_STATTEXT
  "STATTEXT",
#endif
#ifdef wxUSE_STATUSBAR
  "STATUSBAR",
#endif
#ifdef wxUSE_TOGGLEBTN
  "TOGGLEBTN",
#endif
#ifdef wxUSE_TAB_DIALOG
  "TAB_DIALOG",
#endif
#ifdef wxUSE_TABDIALOG
  "TABDIALOG",
#endif
#ifdef wxUSE_TEXTCTRL
  "TEXTCTRL",
#endif
#ifdef wxUSE_TOOLBAR
  "TOOLBAR",
#endif
#ifdef wxUSE_TOOLBAR_NATIVE
  "TOOLBAR_NATIVE",
#endif
#ifdef wxUSE_TOOLBAR_SIMPLE
  "TOOLBAR_SIMPLE",
#endif
#ifdef wxUSE_BUTTONBAR
  "BUTTONBAR",
#endif
#ifdef wxUSE_TREELAYOUT
  "TREELAYOUT",
#endif
#ifdef wxUSE_TREECTRL
  "TREECTRL",
#endif
#ifdef wxUSE_LONGLONG
  "LONGLONG",
#endif
#ifdef wxUSE_GEOMETRY
  "GEOMETRY",
#endif
#ifdef wxUSE_CMDLINE_PARSER
  "CMDLINE_PARSER",
#endif
#ifdef wxUSE_DATETIME
  "DATETIME",
#endif
#ifdef wxUSE_FILE
  "FILE",
#endif
#ifdef wxUSE_FFILE
  "FFILE",
#endif
#ifdef wxUSE_FSVOLUME
  "FSVOLUME",
#endif
#ifdef wxUSE_TEXTBUFFER
  "TEXTBUFFER",
#endif
#ifdef wxUSE_TEXTFILE
  "TEXTFILE",
#endif
#ifdef wxUSE_LOG
  "LOG",
#endif
#ifdef wxUSE_LOGWINDOW
  "LOGWINDOW",
#endif
#ifdef wxUSE_LOGGUI
  "LOGGUI",
#endif
#ifdef wxUSE_LOG_DIALOG
  "LOG_DIALOG",
#endif
#ifdef wxUSE_STOPWATCH
  "STOPWATCH",
#endif
#ifdef wxUSE_TIMEDATE
  "TIMEDATE",
#endif
#ifdef wxUSE_WAVE
  "WAVE",
#endif
#ifdef wxUSE_CONFIG
  "CONFIG",
#endif
#ifdef wxUSE_FONTMAP
  "FONTMAP",
#endif
#ifdef wxUSE_INTL
  "INTL",
#endif
#ifdef wxUSE_PROTOCOL
  "PROTOCOL",
#endif
#ifdef wxUSE_PROTOCOL_FILE
  "PROTOCOL_FILE",
#endif
#ifdef wxUSE_PROTOCOL_FTP
  "PROTOCOL_FTP",
#endif
#ifdef wxUSE_PROTOCOL_HTTP
  "PROTOCOL_HTTP",
#endif
#ifdef wxUSE_STREAMS
  "STREAMS",
#endif
#ifdef wxUSE_SOCKETS
  "SOCKETS",
#endif
#ifdef wxUSE_DIALUP_MANAGER
  "DIALUP_MANAGER",
#endif
#ifdef wxUSE_STD_IOSTREAM
  "STD_IOSTREAM",
#endif
#ifdef wxUSE_DYNLIB_CLASS
  "DYNLIB_CLASS",
#endif
#ifdef wxUSE_DYNAMIC_LOADER
  "DYNAMIC_LOADER",
#endif
#ifdef wxUSE_TIMER
  "TIMER",
#endif
#ifdef wxUSE_AFM_FOR_POSTSCRIPT
  "AFM_FOR_POSTSCRIPT",
#endif
#ifdef wxUSE_NORMALIZED_PS_FONTS
  "NORMALIZED_PS_FONTS",
#endif
#ifdef wxUSE_POSTSCRIPT
  "POSTSCRIPT",
#endif
#ifdef wxUSE_WCHAR_T
  "WCHAR_T",
#endif
#ifdef wxUSE_UNICODE
  "UNICODE",
#endif
#ifdef wxUSE_UNICODE_MSLU
  "UNICODE_MSLU",
#endif
#ifdef wxUSE_URL
  "URL",
#endif
#ifdef wxUSE_WCSRTOMBS
  "WCSRTOMBS",
#endif
#ifdef wxUSE_EXPERIMENTAL_
  "EXPERIMENTAL_PRINTF",
#endif
#ifdef wxUSE_IPC
  "IPC",
#endif
#ifdef wxUSE_X_RESOURCES
  "X_RESOURCES",
#endif
#ifdef wxUSE_CLIPBOARD
  "CLIPBOARD",
#endif
#ifdef wxUSE_DATAOBJ
  "DATAOBJ",
#endif
#ifdef wxUSE_TOOLTIPS
  "TOOLTIPS",
#endif
#ifdef wxUSE_DRAG_AND_DROP
  "DRAG_AND_DROP",
#endif
#ifdef wxUSE_OLE
  "OLE",
#endif
#ifdef wxUSE_SPLINES
  "SPLINES",
#endif
#ifdef wxUSE_MDI_ARCHITECTURE
  "MDI_ARCHITECTURE",
#endif
#ifdef wxUSE_DOC_VIEW_ARCHITECTURE
  "DOC_VIEW_ARCHITECTURE",
#endif
#ifdef wxUSE_PRINTING_ARCHITECTURE
  "PRINTING_ARCHITECTURE",
#endif
#ifdef wxUSE_PROLOGIO
  "PROLOGIO",
#endif
#ifdef wxUSE_RESOURCES
  "RESOURCES",
#endif
#ifdef wxUSE_WX_RESOURCES
  "WX_RESOURCES",
#endif
#ifdef wxUSE_HELP
  "HELP",
#endif
#ifdef wxUSE_WXHTML_HELP
  "WXHTML_HELP",
#endif
#ifdef wxUSE_MS_HTML_HELP
  "MS_HTML_HELP",
#endif
#ifdef wxUSE_IOSTREAMH
  "IOSTREAMH",
#endif
#ifdef wxUSE_APPLE_IEEE
  "APPLE_IEEE",
#endif
#ifdef wxUSE_MEMORY_TRACING
  "MEMORY_TRACING",
#endif
#ifdef wxUSE_DEBUG_NEW_ALWAYS
  "DEBUG_NEW_ALWAYS",
#endif
#ifdef wxUSE_DEBUG_CONTEXT
  "DEBUG_CONTEXT",
#endif
#ifdef wxUSE_GLOBAL_MEMORY_OPERATORS
  "GLOBAL_MEMORY_OPERATORS",
#endif
#ifdef wxUSE_SPLINES
  "SPLINES",
#endif
#ifdef wxUSE_DYNAMIC_CLASSES
  "DYNAMIC_CLASSES",
#endif
#ifdef wxUSE_METAFILE
  "METAFILE",
#endif
#ifdef wxUSE_ENH_METAFILE
  "ENH_METAFILE",
#endif
#ifdef wxUSE_MINIFRAME
  "MINIFRAME",
#endif
#ifdef wxUSE_HTML
  "HTML",
#endif
#ifdef wxUSE_FILESYSTEM
  "FILESYSTEM",
#endif
#ifdef wxUSE_FS_INET
  "FS_INET",
#endif
#ifdef wxUSE_FS_ZIP
  "FS_ZIP",
#endif
#ifdef wxUSE_BUSYINFO
  "BUSYINFO",
#endif
#ifdef wxUSE_ZIPSTREAM
  "ZIPSTREAM",
#endif
#ifdef wxUSE_PALETTE
  "PALETTE",
#endif
#ifdef wxUSE_IMAGE
  "IMAGE",
#endif
#ifdef wxUSE_GIF
  "GIF",
#endif
#ifdef wxUSE_PCX
  "PCX",
#endif
#ifdef wxUSE_IFF
  "IFF",
#endif
#ifdef wxUSE_PNM
  "PNM",
#endif
#ifdef wxUSE_XPM
  "XPM",
#endif
#ifdef wxUSE_ICO_CUR
  "ICO_CUR",
#endif
  NULL
};


static const char* hasDefs[] = {
#ifdef wxHAS_RADIO_MENU_ITEMS
  "RADIO_MENU_ITEMS",
#endif
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

EWXWEXPORT(int, wxIsDefined)( char* s )
{
  int i;
  if (s==NULL) return 0;
  /* check define */
  for( i=0; defineDefs[i] != NULL; i++ ) {
    if (strcmp(s,defineDefs[i]) == 0) return 1;
  }
  /* check wxUSE_XXX */
  if (strncmp(s,"wxUSE_",6) == 0) {
    const char* t = s+6;
    for( i=0; useDefs[i] != NULL; i++ ) {
      if (strcmp(t,useDefs[i]) == 0) return 1;
    }
  }
  /* check wxHAS_XXX */
  if (strncmp(s,"wxHAS_",6) == 0) {
    const char* t = s+6;
    for( i=0; hasDefs[i] != NULL; i++ ) {
      if (strcmp(t,hasDefs[i]) == 0) return 1;
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
EWXWEXPORT(wxString*, wxString_Create)( wxChar* buffer )
{
  return new wxString(buffer);
}

EWXWEXPORT(wxString*, wxString_CreateLen)( wxChar* buffer, int len )
{
  return new wxString(buffer,len);
}

EWXWEXPORT(void,wxString_Delete)( wxString* s )
{
  delete s;
}

EWXWEXPORT(int,wxString_GetString)( wxString* s, wxChar* buffer )
{
  if (buffer) memcpy (buffer, s->c_str(), s->Length() * sizeof(wxChar));
  return s->Length();
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


EWXWEXPORT(void, wxWindow_SetVirtualSize)(void* _obj, int w, int h )
{
    ((wxWindow*)_obj)->SetVirtualSize( w, h );
}

EWXWEXPORT(void, wxWindow_GetVirtualSize)(void* _obj, int* w, int* h )
{
    ((wxWindow*)_obj)->GetVirtualSize( w, h );
}

EWXWEXPORT(void, wxWindow_FitInside)(void* _obj)
{
    ((wxWindow*)_obj)->FitInside();
}


EWXWEXPORT(void, wxWindow_ClientToScreen)(wxWindow* self, int x, int y, int* sx, int* sy)
{
  wxPoint pt = self->ClientToScreen( wxPoint(x,y) );
  if (sx) *sx = pt.x;
  if (sy) *sy = pt.y;
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
  return wxGetNumberFromUser(message, prompt, caption, value, min , max, parent, wxPoint(x,y) );
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
