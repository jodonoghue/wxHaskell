#ifndef __ELJSCINTILLA_H
#define __ELJSCINTILLA_H

#ifdef __WIN32__
#include "wx/msw/private.h"
#include <commctrl.h>
#else
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#if GTK_MAJOR_VERSION < 2
#define SCINTILLA_NOTIFY "notify"
#else
#define SCINTILLA_NOTIFY "sci-notify"
#endif
extern "C"
{
typedef unsigned long uptr_t;
typedef long sptr_t;

GtkWidget*	scintilla_new		(void);
void		scintilla_set_id	(GtkWidget*, int id);
sptr_t	scintilla_send_message	(GtkWidget*, unsigned int iMessage, uptr_t wParam, sptr_t lParam);
}
#endif

extern "C"
{

typedef int  _cdecl (*ScNotify)(void*, void*);
typedef int  _cdecl (*ScExec)  (void*, int, int, int);

}

class wxScintilla : public wxControl
{
DECLARE_DYNAMIC_CLASS(wxScintilla)

public:
    wxScintilla() { }
    wxScintilla(wxWindow *parent, wxWindowID id,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxT("wxScintilla"))
    {
        Create(parent, id, pos, size, style, validator, name);
    }

    bool Create(wxWindow *parent, wxWindowID id,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxT("wxScintilla"));

    static wxSize GetDefaultSize();

#ifdef __WIN32__
	bool MSWOnNotify(int idCtrl, WXLPARAM lParam, WXLPARAM* result);
#else
	static void gtk_scintilla_notify_callback(GtkWidget*, gint, gpointer lParam, wxScintilla* scitew);
#endif

	ScExec m_proc;
	void* m_obj;

	ScNotify sc_eiffel;
	void* sc_object;
	
	void SetEiffelCB (void* _obj, void* _fnc) {sc_object = _obj; sc_eiffel = (ScNotify)_fnc;};

protected:
    virtual wxSize DoGetBestSize() const
	{return GetDefaultSize();};

private:
	bool EnsureDll();
};

#endif
