#include "wrapper.h"
#include <wx/xrc/xmlres.h>

#ifdef wxUSE_STC
# include "wx/stc/stc.h"
#endif

class wxMDIParentFrameXmlHandler : public wxXmlResourceHandler
{
public:
    wxMDIParentFrameXmlHandler();
    virtual wxObject *DoCreateResource();
    virtual bool CanHandle(wxXmlNode *node);
};

class wxMDIChildFrameXmlHandler : public wxXmlResourceHandler
{
public:
    wxMDIChildFrameXmlHandler();
    virtual wxObject *DoCreateResource();
    virtual bool CanHandle(wxXmlNode *node);
};

class wxSplitterWindowXmlHandler : public wxXmlResourceHandler
{
public:
    wxSplitterWindowXmlHandler();
    virtual wxObject *DoCreateResource();
    virtual bool CanHandle(wxXmlNode *node);
};

#ifdef wxUSE_STC
class wxStyledTextCtrlXmlHandler : public wxXmlResourceHandler
{
public:
    wxStyledTextCtrlXmlHandler();
    virtual wxObject *DoCreateResource();
    virtual bool CanHandle(wxXmlNode *node);
};
#endif

class wxGridXmlHandler : public wxXmlResourceHandler
{
public:
    wxGridXmlHandler();
    virtual wxObject *DoCreateResource();
    virtual bool CanHandle(wxXmlNode *node);
};

wxMDIParentFrameXmlHandler::wxMDIParentFrameXmlHandler() : wxXmlResourceHandler()
{
    XRC_ADD_STYLE(wxSTAY_ON_TOP);
    XRC_ADD_STYLE(wxCAPTION);
    XRC_ADD_STYLE(wxDEFAULT_DIALOG_STYLE);
    XRC_ADD_STYLE(wxDEFAULT_FRAME_STYLE);
    XRC_ADD_STYLE(wxSYSTEM_MENU);
    XRC_ADD_STYLE(wxRESIZE_BORDER);

    XRC_ADD_STYLE(wxFRAME_TOOL_WINDOW);
    XRC_ADD_STYLE(wxFRAME_FLOAT_ON_PARENT);
    XRC_ADD_STYLE(wxMAXIMIZE_BOX);
    XRC_ADD_STYLE(wxMINIMIZE_BOX);
    XRC_ADD_STYLE(wxSTAY_ON_TOP);

    XRC_ADD_STYLE(wxTAB_TRAVERSAL);
    XRC_ADD_STYLE(wxWS_EX_VALIDATE_RECURSIVELY);
    XRC_ADD_STYLE(wxCLIP_CHILDREN);

    AddWindowStyles();
}

wxObject *wxMDIParentFrameXmlHandler::DoCreateResource()
{
    XRC_MAKE_INSTANCE(frame, wxMDIParentFrame);

    frame->Create(m_parentAsWindow,
                  GetID(),
                  GetText(wxT("title")),
                  wxDefaultPosition, wxDefaultSize,
                  GetStyle(wxT("style"), wxDEFAULT_FRAME_STYLE),
                  GetName());

    if (HasParam(wxT("size")))
        frame->SetClientSize(GetSize());
    if (HasParam(wxT("pos")))
        frame->Move(GetPosition());

    SetupWindow(frame);

    CreateChildren(frame);

    if (GetBool(wxT("centered"), FALSE))
        frame->Centre();

    return frame;
}

bool wxMDIParentFrameXmlHandler::CanHandle(wxXmlNode *node)
{
    return IsOfClass(node, wxT("wxMDIParentFrame"));
}

wxMDIChildFrameXmlHandler::wxMDIChildFrameXmlHandler() : wxXmlResourceHandler()
{
    XRC_ADD_STYLE(wxSTAY_ON_TOP);
    XRC_ADD_STYLE(wxCAPTION);
    XRC_ADD_STYLE(wxDEFAULT_DIALOG_STYLE);
    XRC_ADD_STYLE(wxDEFAULT_FRAME_STYLE);
    XRC_ADD_STYLE(wxSYSTEM_MENU);
    XRC_ADD_STYLE(wxRESIZE_BORDER);

    XRC_ADD_STYLE(wxFRAME_TOOL_WINDOW);
    XRC_ADD_STYLE(wxFRAME_FLOAT_ON_PARENT);
    XRC_ADD_STYLE(wxMAXIMIZE_BOX);
    XRC_ADD_STYLE(wxMINIMIZE_BOX);
    XRC_ADD_STYLE(wxSTAY_ON_TOP);

    XRC_ADD_STYLE(wxTAB_TRAVERSAL);
    XRC_ADD_STYLE(wxWS_EX_VALIDATE_RECURSIVELY);
    XRC_ADD_STYLE(wxCLIP_CHILDREN);

    AddWindowStyles();
}

wxObject *wxMDIChildFrameXmlHandler::DoCreateResource()
{
    XRC_MAKE_INSTANCE(frame, wxMDIChildFrame);
	
	wxMDIParentFrame* prt = wxDynamicCast (m_parentAsWindow, wxMDIParentFrame);

	if (prt == NULL)
	{
		wxLogError(wxT("Error in resource: wxMDIChildFrame has no wxMDIParentFrame."));
		return NULL;
	}

    frame->Create(prt,
                  GetID(),
                  GetText(wxT("title")),
                  wxDefaultPosition, wxDefaultSize,
                  GetStyle(wxT("style"), wxDEFAULT_FRAME_STYLE),
                  GetName());

    SetupWindow(frame);

    CreateChildren(frame);

    if (GetBool(wxT("centered"), FALSE))
        frame->Centre();

    return frame;
}

bool wxMDIChildFrameXmlHandler::CanHandle(wxXmlNode *node)
{
    return IsOfClass(node, wxT("wxMDIChildFrame"));
}

wxSplitterWindowXmlHandler::wxSplitterWindowXmlHandler() : wxXmlResourceHandler()
{
    XRC_ADD_STYLE(wxSP_3D);
    XRC_ADD_STYLE(wxSP_3DSASH);
    XRC_ADD_STYLE(wxSP_BORDER);
    XRC_ADD_STYLE(wxSP_NOBORDER);
    XRC_ADD_STYLE(wxSP_PERMIT_UNSPLIT);
    XRC_ADD_STYLE(wxSP_LIVE_UPDATE);

    XRC_ADD_STYLE(wxTAB_TRAVERSAL);
    XRC_ADD_STYLE(wxCLIP_CHILDREN);

    AddWindowStyles();
}

wxObject *wxSplitterWindowXmlHandler::DoCreateResource()
{
    XRC_MAKE_INSTANCE(frame, wxSplitterWindow);

    frame->Create(m_parentAsWindow,
                  GetID(),
                  wxDefaultPosition, wxDefaultSize,
                  GetStyle(wxT("style"), wxSP_3D),
                  GetName());

    SetupWindow(frame);

    CreateChildren(frame);
	
	if (frame->GetChildren().GetCount() != 2)
	{
		wxLogError(wxT("Error in resource: Splitter window needs exactly two children."));
		return NULL;
	}
	
	frame->SetSplitMode(GetLong (wxT("splitmode"), wxSPLIT_VERTICAL));
	long sashpos = GetLong (wxT("sashposition"), 100);
	
	wxWindowList::Node* node = frame->GetChildren().GetFirst();
	wxWindow* wnd1 = node->GetData();
	wxWindow* wnd2 = node->GetNext()->GetData();

	if (frame->GetSplitMode() == wxSPLIT_VERTICAL)
		frame->SplitVertically (wnd1, wnd2, sashpos);
	else
		frame->SplitHorizontally (wnd1, wnd2, sashpos);
	
    return frame;
}

bool wxSplitterWindowXmlHandler::CanHandle(wxXmlNode *node)
{
    return IsOfClass(node, wxT("wxSplitterWindow"));
}

#ifdef wxUSE_STC
wxStyledTextCtrlXmlHandler::wxStyledTextCtrlXmlHandler() : wxXmlResourceHandler()
{
    AddWindowStyles();
}

wxObject *wxStyledTextCtrlXmlHandler::DoCreateResource()
{
    XRC_MAKE_INSTANCE(frame, wxStyledTextCtrl);
	
    frame->Create(m_parentAsWindow,
                  GetID(),
                  wxDefaultPosition, wxDefaultSize,
                  GetStyle(wxT("style"), 0),
                  GetName());

    if (HasParam(wxT("size")))
        frame->SetSize(GetSize());
    if (HasParam(wxT("pos")))
        frame->Move(GetPosition());

    SetupWindow(frame);

    return frame;
}

bool wxStyledTextCtrlXmlHandler::CanHandle(wxXmlNode *node)
{
    return IsOfClass(node, wxT("wxStyledTextCtrl"));
}
#endif

wxGridXmlHandler::wxGridXmlHandler() : wxXmlResourceHandler()
{
    XRC_ADD_STYLE(wxTAB_TRAVERSAL);
    XRC_ADD_STYLE(wxCLIP_CHILDREN);

    AddWindowStyles();
}

wxObject *wxGridXmlHandler::DoCreateResource()
{
	wxGrid* grid = new wxGrid(m_parentAsWindow,
                              GetID(),
                              wxDefaultPosition, wxDefaultSize,
                              GetStyle(wxT("style"), wxWANTS_CHARS),
                              GetName());

	long cols = GetLong (wxT("numcols"), 0);
	long rows = GetLong (wxT("numrows"), 0);
	
	if (cols && rows)
		grid->CreateGrid(cols, rows, (wxGrid::wxGridSelectionModes)GetLong (wxT("selmode"), 0));
	
    if (HasParam(wxT("size")))
        grid->SetSize(GetSize());
    if (HasParam(wxT("pos")))
        grid->Move(GetPosition());

	SetupWindow(grid);

	return grid;
}

bool wxGridXmlHandler::CanHandle(wxXmlNode *node)
{
    return IsOfClass(node, wxT("wxGrid"));
}

extern "C"
{

EWXWEXPORT(wxXmlResource*,wxXmlResource_Create)(int flags)
{
	return new wxXmlResource(flags);
}
	
EWXWEXPORT(wxXmlResource*,wxXmlResource_CreateFromFile)(wxString* filemask, int flags)
{
	return new wxXmlResource(*filemask, flags);
}
	
EWXWEXPORT(void,wxXmlResource_Delete)(wxXmlResource* self)
{
	delete self;
}
	
EWXWEXPORT(bool,wxXmlResource_Load)(wxXmlResource* self, wxString* filemask)
{
	wxGetApp().InitZipFileSystem();
	return self->Load(*filemask);
}
	
EWXWEXPORT(void,wxXmlResource_InitAllHandlers)(wxXmlResource* self)
{
	self->InitAllHandlers();
	self->AddHandler(new wxMDIParentFrameXmlHandler());
	self->AddHandler(new wxMDIChildFrameXmlHandler());
	self->AddHandler(new wxSplitterWindowXmlHandler());
#ifdef wxUSE_STC
	self->AddHandler(new wxStyledTextCtrlXmlHandler());
#endif
	self->AddHandler(new wxGridXmlHandler());
}
	
EWXWEXPORT(void,wxXmlResource_AddHandler)(wxXmlResource* self, wxXmlResourceHandler* handler)
{
	self->AddHandler(handler);
}
	
EWXWEXPORT(void,wxXmlResource_InsertHandler)(wxXmlResource* self, wxXmlResourceHandler* handler)
{
	self->InsertHandler(handler);
}
	
EWXWEXPORT(void,wxXmlResource_ClearHandlers)(wxXmlResource* self)
{
	self->ClearHandlers();
}
	
EWXWEXPORT(void,wxXmlResource_AddSubclassFactory)(wxXmlResource* self, wxXmlSubclassFactory* factory)
{
	self->AddSubclassFactory(factory);
}
	
EWXWEXPORT(wxMenu*,wxXmlResource_LoadMenu)(wxXmlResource* self, wxString* name)
{
	return self->LoadMenu(*name);
}
	
EWXWEXPORT(wxMenuBar*,wxXmlResource_LoadMenuBar)(wxXmlResource* self, wxWindow* parent, wxString* name)
{
	return self->LoadMenuBar(parent, *name);
}
	
EWXWEXPORT(wxToolBar*,wxXmlResource_LoadToolBar)(wxXmlResource* self, wxWindow* parent, wxString* name)
{
	return self->LoadToolBar(parent, *name);
}
	
EWXWEXPORT(wxDialog*,wxXmlResource_LoadDialog)(wxXmlResource* self, wxWindow* parent, wxString* name)
{
	return self->LoadDialog(parent, *name);
}
	
EWXWEXPORT(wxPanel*,wxXmlResource_LoadPanel)(wxXmlResource* self, wxWindow* parent, wxString* name)
{
	return self->LoadPanel(parent, *name);
}
	
EWXWEXPORT(wxFrame*,wxXmlResource_LoadFrame)(wxXmlResource* self, wxWindow* parent, wxString* name)
{
	return self->LoadFrame(parent, *name);
}
	
EWXWEXPORT(wxObject*,wxXmlResource_LoadObject)(wxXmlResource* self, wxWindow* parent, wxString* name, wxString* classname)
{
	return self->LoadObject(parent, *name, *classname);
}
	
EWXWEXPORT(bool,wxXmlResource_InitializeObject)( wxXmlResource* self, wxObject* _inst, wxWindow* parent,
                                                 wxString* name, wxString* classname)
{
	return self->LoadObject(_inst, (wxWindow*)parent, *name, *classname);
}
	
EWXWEXPORT(void,wxXmlResource_LoadBitmap)(wxXmlResource* self, wxString* name, void* _ref)
{
	*((wxBitmap*)_ref) = self->LoadBitmap(*name);
}
	
EWXWEXPORT(void,wxXmlResource_LoadIcon)(wxXmlResource* self, wxString* name, void* _ref)
{
	*((wxIcon*)_ref) = self->LoadIcon(*name);
}
	
EWXWEXPORT(bool,wxXmlResource_Unload)(wxXmlResource* self, wxString* name)
{
	return self->Unload(*name);
}
	
EWXWEXPORT(bool,wxXmlResource_AttachUnknownControl)(wxXmlResource* self, wxString* name, wxWindow* control, wxWindow* parent)
{
	return self->AttachUnknownControl(*name, control, parent);
}
	
EWXWEXPORT(int,wxXmlResource_GetXRCID)(wxXmlResource* self, wxChar* str_id)
{
	return self->GetXRCID(str_id);
}
	
EWXWEXPORT(long,wxXmlResource_GetVersion)(wxXmlResource* self)
{
	return self->GetVersion();
}
	
EWXWEXPORT(int,wxXmlResource_CompareVersion)(wxXmlResource* self, int major, int minor, int release, int revision)
{
	return self->CompareVersion(major, minor, release, revision);
}
	
EWXWEXPORT(wxXmlResource*,wxXmlResource_Get)(wxXmlResource* self)
{
	return self->Get();
}
	
EWXWEXPORT(wxXmlResource*,wxXmlResource_Set)(wxXmlResource* self, wxXmlResource* res)
{
	return self->Set(res);
}

EWXWEXPORT(int,wxXmlResource_GetDomain)(wxXmlResource* self, wxChar* buf)
{
	buf = self->GetDomain();
	return wxStrlen(buf);
}

EWXWEXPORT(void,wxXmlResource_SetDomain)(wxXmlResource* self, wxChar* domain)
{
	self->SetDomain(domain);
}

EWXWEXPORT(int,wxXmlResource_GetFlags)(wxXmlResource* self)
{
	return self->GetFlags();
}
	
EWXWEXPORT(void,wxXmlResource_SetFlags)(wxXmlResource* self, int flags)
{
	self->SetFlags(flags);
}
	
}
