#include "wrapper.h"
#include "eljscintilla.h"


IMPLEMENT_DYNAMIC_CLASS(wxScintilla, wxControl)

#ifdef __WIN32__
bool wxScintilla::EnsureDll()
{
	static bool done = FALSE;

	if (done) return TRUE;

	done = TRUE;
	return (bool)LoadLibrary ("SciLexer.dll");
}

bool wxScintilla::Create(wxWindow *parent,
                      wxWindowID id,
                      const wxPoint& pos,
                      const wxSize& size,
                      long style,
                      const wxValidator& validator,
                      const wxString& name)
{
	sc_eiffel = NULL;
	sc_object = NULL;
	
	if (!EnsureDll())
		return FALSE;
		
    if ( !CreateBase(parent, id, pos, size, style, validator, name) )
        return FALSE;

    parent->AddChild((wxButton *)this);

    long msStyle = WS_VISIBLE | WS_TABSTOP | WS_CHILD /* | WS_CLIPSIBLINGS */ ;

    m_hWnd = (WXHWND)CreateWindowEx
                     (
                      m_windowStyle,
                      /* MakeExtendedStyle(m_windowStyle), */
                      wxT("Scintilla"),
                      wxT(""),
                      msStyle,
                      0, 0, 0, 0,
                      GetWinHwnd(parent),
                      (HMENU)m_windowId,
                      wxGetInstance(),
                      NULL
                     );

    if (m_hWnd == 0)
    {
        wxString msg;
#ifdef __WIN16__
        msg.Printf(wxT("CreateWindowEx failed"));
#else
        msg.Printf(wxT("CreateWindowEx failed with error number %ld"), (long) GetLastError());
#endif
        wxFAIL_MSG(msg);
    }

	m_proc = (ScExec)SendMessage((HWND)m_hWnd, 2184, 0, 0);
	m_obj  = (void*) SendMessage((HWND)m_hWnd, 2185, 0, 0);

    // Subclass again for purposes of dialog editing mode
    SubclassWin(m_hWnd);

    SetSize(pos.x, pos.y, size.x, size.y);

    return TRUE;
}

wxSize wxScintilla::GetDefaultSize()
{
    static wxSize s_sizeSci;

    if ( s_sizeSci.x == 0 )
    {
		s_sizeSci.x = 200;
		s_sizeSci.y = 200;
	}
	
	return s_sizeSci;

}

bool wxScintilla::MSWOnNotify(int idCtrl,
                              WXLPARAM lParam,
                              WXLPARAM* result)
{
    wxCommandEvent event(wxEVT_NULL, m_windowId);
    wxEventType eventType = wxEVT_NULL;
    NMHDR *hdr1 = (NMHDR*) lParam;
    switch ( hdr1->code )
    {
        case NM_CLICK:
            eventType = wxEVT_COMMAND_LEFT_CLICK;
            break;

        case NM_DBLCLK:
            eventType = wxEVT_COMMAND_LEFT_DCLICK;
            break;

        case NM_RCLICK:
            eventType = wxEVT_COMMAND_RIGHT_CLICK;
            break;

        case NM_RDBLCLK:
            eventType = wxEVT_COMMAND_RIGHT_DCLICK;
            break;

        case NM_SETFOCUS:
            eventType = wxEVT_COMMAND_SET_FOCUS;
            break;

        case NM_KILLFOCUS:
            eventType = wxEVT_COMMAND_KILL_FOCUS;
            break;

        case NM_RETURN:
            eventType = wxEVT_COMMAND_ENTER;
            break;

        default:
			if (hdr1->hwndFrom == (void*)m_hWnd)
				if (sc_eiffel && sc_object)
					return sc_eiffel(sc_object, (void*)lParam) != 0;
				else
					break;
			else
            	return wxWindow::MSWOnNotify(idCtrl, lParam, result);
    }

    event.SetEventType(eventType);
    event.SetEventObject(this);

    return GetEventHandler()->ProcessEvent(event);
}
#else
bool wxScintilla::EnsureDll()
{
	return FALSE;
}

bool wxScintilla::Create(wxWindow *parent,
                      wxWindowID id,
                      const wxPoint& pos,
                      const wxSize& size,
                      long style,
                      const wxValidator& validator,
                      const wxString& name)
{
	sc_eiffel = NULL;
	sc_object = NULL;

    m_needParent = TRUE;
    m_acceptsFocus = TRUE;

    if (!PreCreation( parent, pos, size ) ||
        !CreateBase( parent, id, pos, size, style, validator, name ))
    {
        wxFAIL_MSG( wxT("wxScintilla creation failed") );
	    return FALSE;
    }

	m_widget = scintilla_new();
	
	if (m_widget)
	{
		scintilla_set_id (m_widget, id);

		m_proc = (ScExec)scintilla_send_message(m_widget, 2184, 0, 0);
		m_obj  = (void*) scintilla_send_message(m_widget, 2185, 0, 0);

		gtk_signal_connect( GTK_OBJECT(m_widget), SCINTILLA_NOTIFY,
			GTK_SIGNAL_FUNC(wxScintilla::gtk_scintilla_notify_callback), (gpointer*)this );
	  
    	m_parent->DoAddChild( this );
  
    	PostCreation();

	    SetSize(pos.x, pos.y, size.x, size.y);
		
		Show (TRUE);
		
		return TRUE;
	}
	return FALSE;
}

wxSize wxScintilla::GetDefaultSize()
{
    static wxSize s_sizeSci;

    if ( s_sizeSci.x == 0 )
    {
		s_sizeSci.x = 200;
		s_sizeSci.y = 200;
	}
	
	return s_sizeSci;

}

void wxScintilla::gtk_scintilla_notify_callback(GtkWidget*, gint /*wParam*/, gpointer lParam, wxScintilla* wxSci)
{
	if (wxSci->sc_object && wxSci->sc_eiffel)
		wxSci->sc_eiffel(wxSci->sc_object, (void*)lParam);
}

#endif

extern "C"
{

EWXWEXPORT(void*, wxScintilla_Create) (void* obj, void* fnc, void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	wxScintilla* result = new wxScintilla ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
	result->SetEiffelCB(obj, fnc);

	return (void*)result;
}

EWXWEXPORT(int, wxScintilla_Exec) (void* obj, int code, int param1, int param2)
{
	return ((wxScintilla*)obj)->m_proc(((wxScintilla*)obj)->m_obj, code, param1, param2);
}

EWXWEXPORT(void, wxScintilla_SetEiffelCB) (void* obj, void* eif, void* fnc)
{
	((wxScintilla*)obj)->SetEiffelCB(eif, fnc);
}

}
