#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxPrinter_Create)(void* data)
{
	return (void*) new wxPrinter((wxPrintDialogData*)data);
}

EWXWEXPORT(void, wxPrinter_Delete)(void* _obj)
{
	delete (wxPrinter*)_obj;
}

EWXWEXPORT(void*, wxPrinter_CreateAbortWindow)(void* _obj, void* parent, void* printout)
{
	return (void*)((wxPrinter*)_obj)->CreateAbortWindow((wxWindow*)parent, (wxPrintout*)printout);
}
	
EWXWEXPORT(void, wxPrinter_ReportError)(void* _obj, void* parent, void* printout, void* message)
{
	((wxPrinter*)_obj)->ReportError((wxWindow*)parent, (wxPrintout*)printout, (char*)message);
}
	
EWXWEXPORT(void, wxPrinter_GetPrintDialogData)(void* _obj, void* _ref)
{
	*((wxPrintDialogData*)_ref) = ((wxPrinter*)_obj)->GetPrintDialogData();
}
	
EWXWEXPORT(int, wxPrinter_GetAbort)(void* _obj)
{
	return (int)((wxPrinter*)_obj)->GetAbort();
}
	
EWXWEXPORT(int, wxPrinter_GetLastError)(void* _obj)
{
	return ((wxPrinter*)_obj)->GetLastError();
}
	
EWXWEXPORT(int, wxPrinter_Setup)(void* _obj, void* parent)
{
	return (int)((wxPrinter*)_obj)->Setup((wxWindow*)parent);
}
	
EWXWEXPORT(int, wxPrinter_Print)(void* _obj, void* parent, void* printout, int prompt)
{
	return (int)((wxPrinter*)_obj)->Print((wxWindow*)parent, (wxPrintout*)printout, prompt != 0);
}
	
EWXWEXPORT(void*, wxPrinter_PrintDialog)(void* _obj, void* parent)
{
	return (void*)((wxPrinter*)_obj)->PrintDialog((wxWindow*)parent);
}
	
EWXWEXPORT(void*, ELJPrintout_Create)(void* title, void* _obj, void* _DoOnBeginDocument, void* _DoOnEndDocument, void* _DoOnBeginPrinting, void* _DoOnEndPrinting, void* _DoOnPreparePrinting, void* _DoOnPrintPage, void* _DoOnHasPage, void* DoOnPageInfo)
{
	return (void*) new ELJPrintout( title, _obj, _DoOnBeginDocument, _DoOnEndDocument, _DoOnBeginPrinting, _DoOnEndPrinting, _DoOnPreparePrinting, _DoOnPrintPage, _DoOnHasPage, DoOnPageInfo);
}
EWXWEXPORT(void, ELJPrintout_Delete)(void* _obj)
{
	delete (ELJPrintout*)_obj;
}

EWXWEXPORT(int, ELJPrintout_GetTitle)(void* _obj, void* _buf)
{
	wxString title = ((ELJPrintout*)_obj)->GetTitle();
	if (_buf) strncpy ((char*)_buf, title.c_str(), title.Length());
	return title.Length();
}
	
EWXWEXPORT(void*, ELJPrintout_GetDC)(void* _obj)
{
	return (void*)((ELJPrintout*)_obj)->GetDC();
}
	
EWXWEXPORT(void, ELJPrintout_SetDC)(void* _obj, void* dc)
{
	((ELJPrintout*)_obj)->SetDC((wxDC*)dc);
}
	
EWXWEXPORT(void, ELJPrintout_SetPageSizePixels)(void* _obj, int w, int  h)
{
	((ELJPrintout*)_obj)->SetPageSizePixels(w, h);
}
	
EWXWEXPORT(void, ELJPrintout_GetPageSizePixels)(void* _obj, void* w, void* h)
{
	((ELJPrintout*)_obj)->GetPageSizePixels((int*)w, (int*)h);
}
	
EWXWEXPORT(void, ELJPrintout_SetPageSizeMM)(void* _obj, int w, int  h)
{
	((ELJPrintout*)_obj)->SetPageSizeMM(w, h);
}
	
EWXWEXPORT(void, ELJPrintout_GetPageSizeMM)(void* _obj, void* w, void* h)
{
	((ELJPrintout*)_obj)->GetPageSizeMM((int*)w, (int*)h);
}
	
EWXWEXPORT(void, ELJPrintout_SetPPIScreen)(void* _obj, int x, int y)
{
	((ELJPrintout*)_obj)->SetPPIScreen(x, y);
}
	
EWXWEXPORT(void, ELJPrintout_GetPPIScreen)(void* _obj, void* x, void* y)
{
	((ELJPrintout*)_obj)->GetPPIScreen((int*)x, (int*)y);
}
	
EWXWEXPORT(void, ELJPrintout_SetPPIPrinter)(void* _obj, int x, int y)
{
	((ELJPrintout*)_obj)->SetPPIPrinter(x, y);
}
	
EWXWEXPORT(void, ELJPrintout_GetPPIPrinter)(void* _obj, void* x, void* y)
{
	((ELJPrintout*)_obj)->GetPPIPrinter((int*)x, (int*)y);
}
	
EWXWEXPORT(int, ELJPrintout_IsPreview)(void* _obj)
{
	return (int)((ELJPrintout*)_obj)->IsPreview();
}
	
EWXWEXPORT(void, ELJPrintout_SetIsPreview)(void* _obj, int p)
{
	((ELJPrintout*)_obj)->SetIsPreview(p != 0);
}

EWXWEXPORT(void*, wxPreviewCanvas_Create) (void* preview, void* parent, int x, int y, int w, int h, int style)
{
	return (void*) new wxPreviewCanvas(	(wxPrintPreviewBase*)preview,
										(wxWindow*)parent,
                    					wxPoint(x, y),
                    					wxSize(w, h),
                    					(long)style);
}

EWXWEXPORT(void*, ELJPreviewFrame_Create) (void* _obj, void* _init, void* _create_canvas, void* _create_toolbar, void* preview, void* parent, void* title,int x, int y,int w, int h, int style)
{
    return (void*) new ELJPreviewFrame(_obj, _init, _create_canvas, _create_toolbar, preview, parent, title, x, y, w, h, style);
}

EWXWEXPORT(void, ELJPreviewFrame_Initialize) (void* _obj)
{
	((ELJPreviewFrame*)_obj)->Initialize();
}
	
EWXWEXPORT(void, ELJPreviewFrame_SetPreviewCanvas) (void* _obj, void* obj)
{
	((ELJPreviewFrame*)_obj)->SetPreviewCanvas (obj);
}
	
EWXWEXPORT(void, ELJPreviewFrame_SetControlBar) (void* _obj, void* obj)
{
	((ELJPreviewFrame*)_obj)->SetControlBar (obj);
}
	
EWXWEXPORT(void, ELJPreviewFrame_SetPrintPreview) (void* _obj, void* obj)
{
	((ELJPreviewFrame*)_obj)->SetPrintPreview (obj);
}
	
EWXWEXPORT(void*, ELJPreviewFrame_GetPreviewCanvas) (void* _obj)
{
	return (void*)((ELJPreviewFrame*)_obj)->GetPreviewCanvas ();
}
	
EWXWEXPORT(void*, ELJPreviewFrame_GetControlBar) (void* _obj)
{
	return (void*)((ELJPreviewFrame*)_obj)->GetControlBar ();
}
	
EWXWEXPORT(void*, ELJPreviewFrame_GetPrintPreview) (void* _obj)
{
	return (void*)((ELJPreviewFrame*)_obj)->GetPrintPreview ();
}
	
EWXWEXPORT(void*, ELJPreviewControlBar_Create) (void* preview, int buttons, void* parent, void* title,int x, int y,int w, int h, int style)
{
    return (void*) new wxPreviewControlBar((wxPrintPreviewBase*)preview, (long)buttons, (wxWindow*)parent, wxPoint(x, y), wxSize(w, h), (long)style);
}

EWXWEXPORT(void*, wxPrintPreview_CreateFromDialogData) (void* printout, void* printoutForPrinting, void* data)
{
    return (void*) new wxPrintPreview((wxPrintout*)printout, (wxPrintout*)printoutForPrinting, (wxPrintDialogData*)data);
}

EWXWEXPORT(void*, wxPrintPreview_CreateFromData) (void* printout, void* printoutForPrinting, void* data)
{
    return (void*) new wxPrintPreview((wxPrintout*)printout, (wxPrintout*)printoutForPrinting, (wxPrintData*)data);
}

EWXWEXPORT(void, wxPrintPreview_Delete)(void* _obj)
{
	delete (wxPrintPreview*)_obj;
}

EWXWEXPORT(int, wxPrintPreview_SetCurrentPage)(void* _obj, int pageNum)
{
	return (int)((wxPrintPreview*)_obj)->SetCurrentPage(pageNum);
}
	
EWXWEXPORT(int, wxPrintPreview_GetCurrentPage)(void* _obj)
{
	return ((wxPrintPreview*)_obj)->GetCurrentPage();
}
	
EWXWEXPORT(void, wxPrintPreview_SetPrintout)(void* _obj, void* printout)
{
	((wxPrintPreview*)_obj)->SetPrintout((wxPrintout*)printout);
}
	
EWXWEXPORT(void*, wxPrintPreview_GetPrintout)(void* _obj)
{
	return (void*)((wxPrintPreview*)_obj)->GetPrintout();
}
	
EWXWEXPORT(void*, wxPrintPreview_GetPrintoutForPrinting)(void* _obj)
{
	return (void*)((wxPrintPreview*)_obj)->GetPrintoutForPrinting();
}
	
EWXWEXPORT(void, wxPrintPreview_SetFrame)(void* _obj, void* frame)
{
	((wxPrintPreview*)_obj)->SetFrame((wxFrame*)frame);
}
	
EWXWEXPORT(void, wxPrintPreview_SetCanvas)(void* _obj, void* canvas)
{
	((wxPrintPreview*)_obj)->SetCanvas((wxPreviewCanvas*)canvas);
}
	
EWXWEXPORT(void*, wxPrintPreview_GetFrame)(void* _obj)
{
	return (void*)((wxPrintPreview*)_obj)->GetFrame();
}
	
EWXWEXPORT(void*, wxPrintPreview_GetCanvas)(void* _obj)
{
	return (void*)((wxPrintPreview*)_obj)->GetCanvas();
}
	
EWXWEXPORT(int, wxPrintPreview_PaintPage)(void* _obj, void* canvas, void* dc)
{
	return (int)((wxPrintPreview*)_obj)->PaintPage((wxPreviewCanvas*)canvas, *((wxDC*)dc));
}
	
EWXWEXPORT(int, wxPrintPreview_DrawBlankPage)(void* _obj, void* canvas, void* dc)
{
	return (int)((wxPrintPreview*)_obj)->DrawBlankPage((wxPreviewCanvas*)canvas, *((wxDC*)dc));
}
	
EWXWEXPORT(int, wxPrintPreview_RenderPage)(void* _obj, int pageNum)
{
	return (int)((wxPrintPreview*)_obj)->RenderPage(pageNum);
}
	
EWXWEXPORT(void, wxPrintPreview_GetPrintDialogData)(void* _obj, void* _ref)
{
	*((wxPrintDialogData*)_ref) = ((wxPrintPreview*)_obj)->GetPrintDialogData();
}
	
EWXWEXPORT(void, wxPrintPreview_SetZoom)(void* _obj, int percent)
{
	((wxPrintPreview*)_obj)->SetZoom(percent);
}
	
EWXWEXPORT(int, wxPrintPreview_GetZoom)(void* _obj)
{
	return ((wxPrintPreview*)_obj)->GetZoom();
}
	
EWXWEXPORT(int, wxPrintPreview_GetMaxPage)(void* _obj)
{
	return ((wxPrintPreview*)_obj)->GetMaxPage();
}
	
EWXWEXPORT(int, wxPrintPreview_GetMinPage)(void* _obj)
{
	return ((wxPrintPreview*)_obj)->GetMinPage();
}
	
EWXWEXPORT(int, wxPrintPreview_Ok)(void* _obj)
{
	return (int)((wxPrintPreview*)_obj)->Ok();
}
	
EWXWEXPORT(void, wxPrintPreview_SetOk)(void* _obj, int ok)
{
	((wxPrintPreview*)_obj)->SetOk(ok != 0);
}
	
EWXWEXPORT(int, wxPrintPreview_Print)(void* _obj, int interactive)
{
	return (int)((wxPrintPreview*)_obj)->Print(interactive != 0);
}
	
EWXWEXPORT(void, wxPrintPreview_DetermineScaling)(void* _obj)
{
	((wxPrintPreview*)_obj)->DetermineScaling();
}
	
EWXWEXPORT(void*, wxPrintData_Create)()
{
	return (void*) new wxPrintData();
}

EWXWEXPORT(void, wxPrintData_Delete)(void* _obj)
{
    delete (wxPrintData*)_obj;
}

EWXWEXPORT(int, wxPrintData_GetNoCopies)(void* _obj)
{
	return ((wxPrintData*)_obj)->GetNoCopies();
}
	
EWXWEXPORT(int, wxPrintData_GetCollate)(void* _obj)
{
	return (int)((wxPrintData*)_obj)->GetCollate();
}
	
EWXWEXPORT(int, wxPrintData_GetOrientation)(void* _obj)
{
	return ((wxPrintData*)_obj)->GetOrientation();
}
	
EWXWEXPORT(int, wxPrintData_GetPrinterName)(void* _obj, void* _ref)
{
	wxString tmp = ((wxPrintData*)_obj)->GetPrinterName();
	if (_ref) strncpy ((char*)_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int, wxPrintData_GetColour)(void* _obj)
{
	return (int)((wxPrintData*)_obj)->GetColour();
}
	
EWXWEXPORT(int, wxPrintData_GetDuplex)(void* _obj)
{
	return (int)((wxPrintData*)_obj)->GetDuplex();
}
	
EWXWEXPORT(int, wxPrintData_GetPaperId)(void* _obj)
{
	return (int)((wxPrintData*)_obj)->GetPaperId();
}
	
EWXWEXPORT(void, wxPrintData_GetPaperSize)(void* _obj, void* w, void* h)
{
	wxSize tmp = ((wxPrintData*)_obj)->GetPaperSize();
	*((int*)w) = tmp.x;
	*((int*)h) = tmp.y;
}
	
EWXWEXPORT(int, wxPrintData_GetQuality)(void* _obj)
{
	return (int)((wxPrintData*)_obj)->GetQuality();
}
	
EWXWEXPORT(void, wxPrintData_SetNoCopies)(void* _obj, int v)
{
	((wxPrintData*)_obj)->SetNoCopies(v);
}
	
EWXWEXPORT(void, wxPrintData_SetCollate)(void* _obj, int flag)
{
	((wxPrintData*)_obj)->SetCollate(flag != 0);
}
	
EWXWEXPORT(void, wxPrintData_SetOrientation)(void* _obj, int orient)
{
	((wxPrintData*)_obj)->SetOrientation(orient);
}
	
EWXWEXPORT(void, wxPrintData_SetPrinterName)(void* _obj, void* name)
{
	((wxPrintData*)_obj)->SetPrinterName((char*)name);
}
	
EWXWEXPORT(void, wxPrintData_SetColour)(void* _obj, int colour)
{
	((wxPrintData*)_obj)->SetColour(colour != 0);
}
	
EWXWEXPORT(void, wxPrintData_SetDuplex)(void* _obj, int duplex)
{
	((wxPrintData*)_obj)->SetDuplex((wxDuplexMode)duplex);
}
	
EWXWEXPORT(void, wxPrintData_SetPaperId)(void* _obj, int sizeId)
{
	((wxPrintData*)_obj)->SetPaperId((wxPaperSize)sizeId);
}
	
EWXWEXPORT(void, wxPrintData_SetPaperSize)(void* _obj, int w, int h)
{
	((wxPrintData*)_obj)->SetPaperSize(wxSize(w, h));
}
	
EWXWEXPORT(void, wxPrintData_SetQuality)(void* _obj, int quality)
{
	((wxPrintData*)_obj)->SetQuality((wxPrintQuality)quality);
}
	
EWXWEXPORT(int, wxPrintData_GetPrinterCommand)(void* _obj, void* _ref)
{
	wxString tmp = ((wxPrintData*)_obj)->GetPrinterCommand();
	if (_ref) strncpy ((char*)_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int, wxPrintData_GetPrinterOptions)(void* _obj, void* _ref)
{
	wxString tmp = ((wxPrintData*)_obj)->GetPrinterOptions();
	if (_ref) strncpy ((char*)_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int, wxPrintData_GetPreviewCommand)(void* _obj, void* _ref)
{
	wxString tmp = ((wxPrintData*)_obj)->GetPreviewCommand();
	if (_ref) strncpy ((char*)_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int, wxPrintData_GetFilename)(void* _obj, void* _ref)
{
	wxString tmp = ((wxPrintData*)_obj)->GetFilename();
	if (_ref) strncpy ((char*)_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(int, wxPrintData_GetFontMetricPath)(void* _obj, void* _ref)
{
	wxString tmp = ((wxPrintData*)_obj)->GetFontMetricPath();
	if (_ref) strncpy ((char*)_ref, tmp.c_str(), tmp.Length());
	return tmp.Length();
}
	
EWXWEXPORT(double, wxPrintData_GetPrinterScaleX)(void* _obj)
{
	return ((wxPrintData*)_obj)->GetPrinterScaleX();
}
	
EWXWEXPORT(double, wxPrintData_GetPrinterScaleY)(void* _obj)
{
	return ((wxPrintData*)_obj)->GetPrinterScaleY();
}
	
EWXWEXPORT(int, wxPrintData_GetPrinterTranslateX)(void* _obj)
{
	return (int)((wxPrintData*)_obj)->GetPrinterTranslateX();
}
	
EWXWEXPORT(int, wxPrintData_GetPrinterTranslateY)(void* _obj)
{
	return (int)((wxPrintData*)_obj)->GetPrinterTranslateY();
}
	
EWXWEXPORT(int, wxPrintData_GetPrintMode)(void* _obj)
{
	return (int)((wxPrintData*)_obj)->GetPrintMode();
}
	
EWXWEXPORT(void, wxPrintData_SetPrinterCommand)(void* _obj, void* command)
{
	((wxPrintData*)_obj)->SetPrinterCommand((char*)command);
}
	
EWXWEXPORT(void, wxPrintData_SetPrinterOptions)(void* _obj, void* options)
{
	((wxPrintData*)_obj)->SetPrinterOptions((char*)options);
}
	
EWXWEXPORT(void, wxPrintData_SetPreviewCommand)(void* _obj, void* command)
{
	((wxPrintData*)_obj)->SetPreviewCommand((char*)command);
}
	
EWXWEXPORT(void, wxPrintData_SetFilename)(void* _obj, void* filename)
{
	((wxPrintData*)_obj)->SetFilename((char*)filename);
}
	
EWXWEXPORT(void, wxPrintData_SetFontMetricPath)(void* _obj, void* path)
{
	((wxPrintData*)_obj)->SetFontMetricPath((char*)path);
}
	
EWXWEXPORT(void, wxPrintData_SetPrinterScaleX)(void* _obj, double x)
{
	((wxPrintData*)_obj)->SetPrinterScaleX(x);
}
	
EWXWEXPORT(void, wxPrintData_SetPrinterScaleY)(void* _obj, double y)
{
	((wxPrintData*)_obj)->SetPrinterScaleY(y);
}
	
EWXWEXPORT(void, wxPrintData_SetPrinterScaling)(void* _obj, double x, double y)
{
	((wxPrintData*)_obj)->SetPrinterScaling(x, y);
}
	
EWXWEXPORT(void, wxPrintData_SetPrinterTranslateX)(void* _obj, int x)
{
	((wxPrintData*)_obj)->SetPrinterTranslateX((int)x);
}
	
EWXWEXPORT(void, wxPrintData_SetPrinterTranslateY)(void* _obj, int y)
{
	((wxPrintData*)_obj)->SetPrinterTranslateY((long)y);
}
	
EWXWEXPORT(void, wxPrintData_SetPrinterTranslation)(void* _obj, int x, int y)
{
	((wxPrintData*)_obj)->SetPrinterTranslation((long)x, (long)y);
}
	
EWXWEXPORT(void, wxPrintData_SetPrintMode)(void* _obj, int printMode)
{
	((wxPrintData*)_obj)->SetPrintMode((wxPrintMode)printMode);
}
	
EWXWEXPORT(void, wxPrintData_Assign)(void* _obj, void* data)
{
	*((wxPrintData*)_obj) = *((wxPrintData*)data);
}
	
EWXWEXPORT(void*, wxPrintDialogData_CreateDefault)()
{
	return (void*) new wxPrintDialogData();
}

EWXWEXPORT(void*, wxPrintDialogData_CreateFromData)(void* printData)
{
	return (void*) new wxPrintDialogData(*((wxPrintData*)printData));
}

EWXWEXPORT(void, wxPrintDialogData_Delete) (void* _obj)
{
	delete (wxPrintDialogData*)_obj;
}

EWXWEXPORT(int, wxPrintDialogData_GetFromPage)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetFromPage();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetToPage)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetToPage();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetMinPage)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetMinPage();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetMaxPage)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetMaxPage();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetNoCopies)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetNoCopies();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetAllPages)(void* _obj)
{
	return (int)((wxPrintDialogData*)_obj)->GetAllPages();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetSelection)(void* _obj)
{
	return (int)((wxPrintDialogData*)_obj)->GetSelection();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetCollate)(void* _obj)
{
	return (int)((wxPrintDialogData*)_obj)->GetCollate();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetPrintToFile)(void* _obj)
{
	return (int)((wxPrintDialogData*)_obj)->GetPrintToFile();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetSetupDialog)(void* _obj)
{
	return (int)((wxPrintDialogData*)_obj)->GetSetupDialog();
}
	
EWXWEXPORT(void, wxPrintDialogData_SetFromPage)(void* _obj, int v)
{
	((wxPrintDialogData*)_obj)->SetFromPage(v);
}
	
EWXWEXPORT(void, wxPrintDialogData_SetToPage)(void* _obj, int v)
{
	((wxPrintDialogData*)_obj)->SetToPage(v);
}
	
EWXWEXPORT(void, wxPrintDialogData_SetMinPage)(void* _obj, int v)
{
	((wxPrintDialogData*)_obj)->SetMinPage(v);
}
	
EWXWEXPORT(void, wxPrintDialogData_SetMaxPage)(void* _obj, int v)
{
	((wxPrintDialogData*)_obj)->SetMaxPage(v);
}
	
EWXWEXPORT(void, wxPrintDialogData_SetNoCopies)(void* _obj, int v)
{
	((wxPrintDialogData*)_obj)->SetNoCopies(v);
}
	
EWXWEXPORT(void, wxPrintDialogData_SetAllPages)(void* _obj, int flag)
{
	((wxPrintDialogData*)_obj)->SetAllPages(flag != 0);
}
	
EWXWEXPORT(void, wxPrintDialogData_SetSelection)(void* _obj, int flag)
{
	((wxPrintDialogData*)_obj)->SetSelection(flag != 0);
}
	
EWXWEXPORT(void, wxPrintDialogData_SetCollate)(void* _obj, int flag)
{
	((wxPrintDialogData*)_obj)->SetCollate(flag != 0);
}
	
EWXWEXPORT(void, wxPrintDialogData_SetPrintToFile)(void* _obj, int flag)
{
	((wxPrintDialogData*)_obj)->SetPrintToFile(flag != 0);
}
	
EWXWEXPORT(void, wxPrintDialogData_SetSetupDialog)(void* _obj, int flag)
{
	((wxPrintDialogData*)_obj)->SetSetupDialog(flag != 0);
}
	
EWXWEXPORT(void, wxPrintDialogData_EnablePrintToFile)(void* _obj, int flag)
{
	((wxPrintDialogData*)_obj)->EnablePrintToFile(flag != 0);
}
	
EWXWEXPORT(void, wxPrintDialogData_EnableSelection)(void* _obj, int flag)
{
	((wxPrintDialogData*)_obj)->EnableSelection(flag != 0);
}
	
EWXWEXPORT(void, wxPrintDialogData_EnablePageNumbers)(void* _obj, int flag)
{
	((wxPrintDialogData*)_obj)->EnablePageNumbers(flag != 0);
}
	
EWXWEXPORT(void, wxPrintDialogData_EnableHelp)(void* _obj, int flag)
{
	((wxPrintDialogData*)_obj)->EnableHelp(flag != 0);
}
	
EWXWEXPORT(int, wxPrintDialogData_GetEnablePrintToFile)(void* _obj)
{
	return (int)((wxPrintDialogData*)_obj)->GetEnablePrintToFile();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetEnableSelection)(void* _obj)
{
	return (int)((wxPrintDialogData*)_obj)->GetEnableSelection();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetEnablePageNumbers)(void* _obj)
{
	return (int)((wxPrintDialogData*)_obj)->GetEnablePageNumbers();
}
	
EWXWEXPORT(int, wxPrintDialogData_GetEnableHelp)(void* _obj)
{
	return (int)((wxPrintDialogData*)_obj)->GetEnableHelp();
}
	
EWXWEXPORT(void, wxPrintDialogData_GetPrintData)(void* _obj, void* _ref)
{
	*((wxPrintData*)_ref) = ((wxPrintDialogData*)_obj)->GetPrintData();
}
	
EWXWEXPORT(void, wxPrintDialogData_SetPrintData)(void* _obj, void* printData)
{
	((wxPrintDialogData*)_obj)->SetPrintData(*((wxPrintData*)printData));
}
	
EWXWEXPORT(void, wxPrintDialogData_Assign)(void* _obj, void* data)
{
	*((wxPrintDialogData*)_obj) = *((wxPrintDialogData*)data);
}
	
EWXWEXPORT(void, wxPrintDialogData_AssignData)(void* _obj, void* data)
{
	*((wxPrintDialogData*)_obj) = *((wxPrintData*)data);
}
	
}
