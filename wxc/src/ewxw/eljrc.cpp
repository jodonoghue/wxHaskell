#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400

#ifdef __WIN32__
#define LoadBitmap LoadBitmapA
#define LoadIcon LoadIconA
#endif

#include "eljscintilla.h"
#include "wx/xrc/xmlres.h"

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

class wxScintillaXmlHandler : public wxXmlResourceHandler
{
public:
    wxScintillaXmlHandler();
    virtual wxObject *DoCreateResource();
    virtual bool CanHandle(wxXmlNode *node);
};

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
    XRC_ADD_STYLE(wxTHICK_FRAME);
    XRC_ADD_STYLE(wxSYSTEM_MENU);
    XRC_ADD_STYLE(wxRESIZE_BORDER);
    XRC_ADD_STYLE(wxRESIZE_BOX);

    XRC_ADD_STYLE(wxFRAME_TOOL_WINDOW);
    XRC_ADD_STYLE(wxFRAME_FLOAT_ON_PARENT);
    XRC_ADD_STYLE(wxMAXIMIZE_BOX);
    XRC_ADD_STYLE(wxMINIMIZE_BOX);
    XRC_ADD_STYLE(wxSTAY_ON_TOP);

    XRC_ADD_STYLE(wxNO_3D);
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
    XRC_ADD_STYLE(wxTHICK_FRAME);
    XRC_ADD_STYLE(wxSYSTEM_MENU);
    XRC_ADD_STYLE(wxRESIZE_BORDER);
    XRC_ADD_STYLE(wxRESIZE_BOX);

    XRC_ADD_STYLE(wxFRAME_TOOL_WINDOW);
    XRC_ADD_STYLE(wxFRAME_FLOAT_ON_PARENT);
    XRC_ADD_STYLE(wxMAXIMIZE_BOX);
    XRC_ADD_STYLE(wxMINIMIZE_BOX);
    XRC_ADD_STYLE(wxSTAY_ON_TOP);

    XRC_ADD_STYLE(wxNO_3D);
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
    XRC_ADD_STYLE(wxSP_FULLSASH);
    XRC_ADD_STYLE(wxSP_BORDER);
    XRC_ADD_STYLE(wxSP_NOBORDER);
    XRC_ADD_STYLE(wxSP_PERMIT_UNSPLIT);
    XRC_ADD_STYLE(wxSP_LIVE_UPDATE);

    XRC_ADD_STYLE(wxNO_3D);
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

wxScintillaXmlHandler::wxScintillaXmlHandler() : wxXmlResourceHandler()
{
    XRC_ADD_STYLE(wxNO_3D);
    AddWindowStyles();
}

wxObject *wxScintillaXmlHandler::DoCreateResource()
{
    XRC_MAKE_INSTANCE(frame, wxScintilla);
	
    frame->Create(m_parentAsWindow,
                  GetID(),
                  wxDefaultPosition, wxDefaultSize,
                  GetStyle(wxT("style"), 0),
				  wxDefaultValidator,
                  GetName());

    if (HasParam(wxT("size")))
        frame->SetSize(GetSize());
    if (HasParam(wxT("pos")))
        frame->Move(GetPosition());

    SetupWindow(frame);

    return frame;
}

bool wxScintillaXmlHandler::CanHandle(wxXmlNode *node)
{
    return IsOfClass(node, wxT("wxScintilla"));
}

wxGridXmlHandler::wxGridXmlHandler() : wxXmlResourceHandler()
{
    XRC_ADD_STYLE(wxNO_3D);
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

EWXWEXPORT(void*,wxXmlResource_Create)(void* _obj, int flags)
{
	return (void*)new wxXmlResource(flags);
}
	
EWXWEXPORT(void*,wxXmlResource_CreateFromFile)(void* _obj, void* filemask, int flags)
{
	return (void*)new wxXmlResource((char*)filemask, flags);
}
	
EWXWEXPORT(void,wxXmlResource_Delete)(void* _obj)
{
	delete (wxXmlResource*)_obj;
}
	
EWXWEXPORT(int,wxXmlResource_Load)(void* _obj, void* filemask)
{
	wxGetApp().InitZipFileSystem();
	return (int)((wxXmlResource*)_obj)->Load((char*)filemask);
}
	
EWXWEXPORT(void,wxXmlResource_InitAllHandlers)(void* _obj)
{
	((wxXmlResource*)_obj)->InitAllHandlers();
	((wxXmlResource*)_obj)->AddHandler(new wxMDIParentFrameXmlHandler());
	((wxXmlResource*)_obj)->AddHandler(new wxMDIChildFrameXmlHandler());
	((wxXmlResource*)_obj)->AddHandler(new wxSplitterWindowXmlHandler());
	((wxXmlResource*)_obj)->AddHandler(new wxScintillaXmlHandler());
	((wxXmlResource*)_obj)->AddHandler(new wxGridXmlHandler());
}
	
EWXWEXPORT(void,wxXmlResource_AddHandler)(void* _obj, void* handler)
{
	((wxXmlResource*)_obj)->AddHandler((wxXmlResourceHandler*)handler);
}
	
EWXWEXPORT(void,wxXmlResource_InsertHandler)(void* _obj, void* handler)
{
	((wxXmlResource*)_obj)->InsertHandler((wxXmlResourceHandler*)handler);
}
	
EWXWEXPORT(void,wxXmlResource_ClearHandlers)(void* _obj)
{
	((wxXmlResource*)_obj)->ClearHandlers();
}
	
EWXWEXPORT(void,wxXmlResource_AddSubclassFactory)(void* _obj, void* factory)
{
	((wxXmlResource*)_obj)->AddSubclassFactory((wxXmlSubclassFactory*)factory);
}
	
EWXWEXPORT(void*,wxXmlResource_LoadMenu)(void* _obj, void* name)
{
	return (void*)((wxXmlResource*)_obj)->LoadMenu((char*)name);
}
	
EWXWEXPORT(void*,wxXmlResource_LoadMenuBar)(void* _obj, void* parent, void* name)
{
	return (void*)((wxXmlResource*)_obj)->LoadMenuBar((wxWindow*)parent, (char*)name);
}
	
EWXWEXPORT(void*,wxXmlResource_LoadToolBar)(void* _obj, void* parent, void* name)
{
	return (void*)((wxXmlResource*)_obj)->LoadToolBar((wxWindow*)parent, (char*)name);
}
	
EWXWEXPORT(void*,wxXmlResource_LoadDialog)(void* _obj, void* parent, void* name)
{
	return (void*)((wxXmlResource*)_obj)->LoadDialog((wxWindow*)parent, (char*)name);
}
	
EWXWEXPORT(void*,wxXmlResource_LoadPanel)(void* _obj, void* parent, void* name)
{
	return (void*)((wxXmlResource*)_obj)->LoadPanel((wxWindow*)parent, (char*)name);
}
	
EWXWEXPORT(void*,wxXmlResource_LoadFrame)(void* _obj, void* parent, void* name)
{
	return (void*)((wxXmlResource*)_obj)->LoadFrame((wxWindow*)parent, (char*)name);
}
	
EWXWEXPORT(void*,wxXmlResource_LoadObject)(void* _obj, void* parent, void* name, void* classname)
{
	return (void*)((wxXmlResource*)_obj)->LoadObject((wxWindow*)parent, (char*)name, (char*)classname);
}
	
EWXWEXPORT(int,wxXmlResource_InitializeObject)(void* _obj, void* _inst, void* parent, void* name, void* classname)
{
	return (int)((wxXmlResource*)_obj)->LoadObject((wxObject*)_inst, (wxWindow*)parent, (char*)name, (char*)classname);
}
	
EWXWEXPORT(void,wxXmlResource_LoadBitmap)(void* _obj, void* name, void* _ref)
{
	*((wxBitmap*)_ref) = ((wxXmlResource*)_obj)->LoadBitmap((char*)name);
}
	
EWXWEXPORT(void,wxXmlResource_LoadIcon)(void* _obj, void* name, void* _ref)
{
	*((wxIcon*)_ref) = ((wxXmlResource*)_obj)->LoadIcon((char*)name);
}
	
EWXWEXPORT(int,wxXmlResource_AttachUnknownControl)(void* _obj, void* name, void* control, void* parent)
{
	return (int)((wxXmlResource*)_obj)->AttachUnknownControl((char*)name, (wxWindow*)control, (wxWindow*)parent);
}
	
EWXWEXPORT(int,wxXmlResource_GetXRCID)(void* str_id)
{
	return wxXmlResource::GetXRCID((wxChar*)str_id);
}
	
EWXWEXPORT(int,wxXmlResource_GetVersion)(void* _obj)
{
	return (int)((wxXmlResource*)_obj)->GetVersion();
}
	
EWXWEXPORT(int,wxXmlResource_CompareVersion)(void* _obj, int major, int minor, int release, int revision)
{
	return ((wxXmlResource*)_obj)->CompareVersion(major, minor, release, revision);
}
	
EWXWEXPORT(void*,wxXmlResource_Get)()
{
	return (void*)wxXmlResource::Get();
}
	
EWXWEXPORT(void*,wxXmlResource_Set)(void* res)
{
	return (void*)wxXmlResource::Set((wxXmlResource*)res);
}
	
EWXWEXPORT(int,wxXmlResource_GetFlags)(void* _obj)
{
	return ((wxXmlResource*)_obj)->GetFlags();
}
	
EWXWEXPORT(void,wxXmlResource_SetFlags)(void* _obj, int flags)
{
	((wxXmlResource*)_obj)->SetFlags(flags);
}
	
}
#endif
