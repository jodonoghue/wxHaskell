#include "wrapper.h"

#if !defined(__WXGTK__)
# include <wx/dcprint.h>
#endif

#if defined(wxUSE_POSTSCRIPT) && (wxUSE_POSTSCRIPT==0)
# undef wxUSE_POSTSCRIPT
#endif

#ifdef wxUSE_POSTSCRIPT
# include <wx/dcps.h>
# include "wx/generic/prntdlgg.h"
#endif

#ifndef wxUSE_POSTSCRIPT
# define wxPostScriptDC        void
#endif

extern "C"
{

EWXWEXPORT(void*,wxPrinter_Create)(void* data)
{
	return (void*) new wxPrinter((wxPrintDialogData*)data);
}

EWXWEXPORT(void,wxPrinter_Delete)(void* _obj)
{
	delete (wxPrinter*)_obj;
}

EWXWEXPORT(void*,wxPrinter_CreateAbortWindow)(void* _obj,wxWindow* parent,void* printout)
{
	return (void*)((wxPrinter*)_obj)->CreateAbortWindow(parent, (wxPrintout*)printout);
}
	
EWXWEXPORT(void,wxPrinter_ReportError)(void* _obj,wxWindow* parent,void* printout,wxString* message)
{
	((wxPrinter*)_obj)->ReportError(parent, (wxPrintout*)printout, *message);
}
	
EWXWEXPORT(void,wxPrinter_GetPrintDialogData)(void* _obj,void* _ref)
{
	*((wxPrintDialogData*)_ref) = ((wxPrinter*)_obj)->GetPrintDialogData();
}
	
EWXWEXPORT(int,wxPrinter_GetAbort)(wxPrinter* _obj)
{
	return (int)_obj->GetAbort();
}
	
EWXWEXPORT(int,wxPrinter_GetLastError)(void* _obj)
{
	return ((wxPrinter*)_obj)->GetLastError();
}
	
EWXWEXPORT(int,wxPrinter_Setup)(wxPrinter* _obj,wxWindow* parent)
{
	return (int)_obj->Setup(parent);
}
	
EWXWEXPORT(int,wxPrinter_Print)(wxPrinter* _obj,wxWindow* parent,wxPrintout* printout,int prompt)
{
	return (int)_obj->Print(parent, printout, prompt != 0);
}
	
EWXWEXPORT(void*,wxPrinter_PrintDialog)(void* _obj,wxWindow* parent)
{
	return (void*)((wxPrinter*)_obj)->PrintDialog(parent);
}
	
EWXWEXPORT(void*,ELJPrintout_Create)(void* title,void* _obj,void* _DoOnBeginDocument,void* _DoOnEndDocument,void* _DoOnBeginPrinting,void* _DoOnEndPrinting,void* _DoOnPreparePrinting,void* _DoOnPrintPage,void* _DoOnHasPage,void* DoOnPageInfo)
{
	return (void*) new ELJPrintout( title, _obj, _DoOnBeginDocument, _DoOnEndDocument, _DoOnBeginPrinting, _DoOnEndPrinting, _DoOnPreparePrinting, _DoOnPrintPage, _DoOnHasPage, DoOnPageInfo);
}
EWXWEXPORT(void,ELJPrintout_Delete)(void* _obj)
{
	delete (ELJPrintout*)_obj;
}

EWXWEXPORT(wxString*,ELJPrintout_GetTitle)(void* _obj)
{
	return new wxString(((ELJPrintout*)_obj)->GetTitle());
}
	
EWXWEXPORT(void*,ELJPrintout_GetDC)(void* _obj)
{
	return (void*)((ELJPrintout*)_obj)->GetDC();
}
	
EWXWEXPORT(void,ELJPrintout_SetDC)(void* _obj,void* dc)
{
	((ELJPrintout*)_obj)->SetDC((wxDC*)dc);
}
	
EWXWEXPORT(void,ELJPrintout_SetPageSizePixels)(void* _obj,int w,int h)
{
	((ELJPrintout*)_obj)->SetPageSizePixels(w, h);
}
	
EWXWEXPORT(void,ELJPrintout_GetPageSizePixels)(void* _obj,int* w,int* h)
{
	((ELJPrintout*)_obj)->GetPageSizePixels(w,h);
}
	
EWXWEXPORT(void,ELJPrintout_SetPageSizeMM)(void* _obj,int w,int h)
{
	((ELJPrintout*)_obj)->SetPageSizeMM(w, h);
}
	
EWXWEXPORT(void,ELJPrintout_GetPageSizeMM)(void* _obj,int* w,int* h)
{
	((ELJPrintout*)_obj)->GetPageSizeMM(w,h);
}
	
EWXWEXPORT(void,ELJPrintout_SetPPIScreen)(void* _obj,int x,int y)
{
	((ELJPrintout*)_obj)->SetPPIScreen(x, y);
}
	
EWXWEXPORT(void,ELJPrintout_GetPPIScreen)(void* _obj,int* x,int* y)
{
	((ELJPrintout*)_obj)->GetPPIScreen(x,y);
}
	
EWXWEXPORT(void,ELJPrintout_SetPPIPrinter)(void* _obj,int x,int y)
{
	((ELJPrintout*)_obj)->SetPPIPrinter(x, y);
}
	
EWXWEXPORT(void,ELJPrintout_GetPPIPrinter)(void* _obj,int* x,int* y)
{
	((ELJPrintout*)_obj)->GetPPIPrinter(x,y);
}
	
EWXWEXPORT(int,ELJPrintout_IsPreview)(ELJPrintout* _obj)
{
	return (int)_obj->IsPreview();
}
	
EWXWEXPORT(void,ELJPrintout_SetIsPreview)(void* _obj,int p)
{
	((ELJPrintout*)_obj)->SetIsPreview(p != 0);
}

EWXWEXPORT(void*,wxPreviewCanvas_Create)(void* preview,wxWindow* parent,int x,int y,int w,int h,int style)
{
	return (void*) new wxPreviewCanvas(	(wxPrintPreviewBase*)preview,parent,
                    				 wxPoint(x, y),wxSize(w, h),
                    					(long)style);
}

EWXWEXPORT(void*,ELJPreviewFrame_Create)(void* _obj,void* _init,void* _create_canvas,void* _create_toolbar,void* preview,void* parent,void* title,int x,int y,int w,int h,int style)
{
    return (void*) new ELJPreviewFrame(_obj, _init, _create_canvas, _create_toolbar, preview, parent, title, x, y, w, h, style);
}

EWXWEXPORT(void,ELJPreviewFrame_Initialize)(void* _obj)
{
	((ELJPreviewFrame*)_obj)->Initialize();
}
	
EWXWEXPORT(void,ELJPreviewFrame_SetPreviewCanvas)(void* _obj,void* obj)
{
	((ELJPreviewFrame*)_obj)->SetPreviewCanvas (obj);
}
	
EWXWEXPORT(void,ELJPreviewFrame_SetControlBar)(void* _obj,void* obj)
{
	((ELJPreviewFrame*)_obj)->SetControlBar (obj);
}
	
EWXWEXPORT(void,ELJPreviewFrame_SetPrintPreview)(void* _obj,void* obj)
{
	((ELJPreviewFrame*)_obj)->SetPrintPreview (obj);
}
	
EWXWEXPORT(void*,ELJPreviewFrame_GetPreviewCanvas)(void* _obj)
{
	return (void*)((ELJPreviewFrame*)_obj)->GetPreviewCanvas ();
}
	
EWXWEXPORT(void*,ELJPreviewFrame_GetControlBar)(void* _obj)
{
	return (void*)((ELJPreviewFrame*)_obj)->GetControlBar ();
}
	
EWXWEXPORT(void*,ELJPreviewFrame_GetPrintPreview)(void* _obj)
{
	return (void*)((ELJPreviewFrame*)_obj)->GetPrintPreview ();
}
	
EWXWEXPORT(void*, ELJPreviewControlBar_Create)(void* preview,int buttons,wxWindow* parent,void* title,int x,int y,int w,int h,int style)
{
    return (void*) new wxPreviewControlBar((wxPrintPreviewBase*)preview, (long)buttons, parent, wxPoint(x, y), wxSize(w, h), (long)style);
}

EWXWEXPORT(void*,wxPrintPreview_CreateFromDialogData)(void* printout,void* printoutForPrinting,void* data)
{
    return (void*) new wxPrintPreview((wxPrintout*)printout, (wxPrintout*)printoutForPrinting, (wxPrintDialogData*)data);
}

EWXWEXPORT(void*,wxPrintPreview_CreateFromData)(void* printout,void* printoutForPrinting,void* data)
{
    return (void*) new wxPrintPreview((wxPrintout*)printout, (wxPrintout*)printoutForPrinting, (wxPrintData*)data);
}

EWXWEXPORT(void,wxPrintPreview_Delete)(void* _obj)
{
	delete (wxPrintPreview*)_obj;
}

EWXWEXPORT(void,wxPrintPreview_SetCurrentPage)(wxPrintPreview* _obj,int pageNum)
{
	_obj->SetCurrentPage(pageNum);
}
	
EWXWEXPORT(int,wxPrintPreview_GetCurrentPage)(void* _obj)
{
	return ((wxPrintPreview*)_obj)->GetCurrentPage();
}
	
EWXWEXPORT(void,wxPrintPreview_SetPrintout)(void* _obj,void* printout)
{
	((wxPrintPreview*)_obj)->SetPrintout((wxPrintout*)printout);
}
	
EWXWEXPORT(void*,wxPrintPreview_GetPrintout)(void* _obj)
{
	return (void*)((wxPrintPreview*)_obj)->GetPrintout();
}
	
EWXWEXPORT(void*,wxPrintPreview_GetPrintoutForPrinting)(void* _obj)
{
	return (void*)((wxPrintPreview*)_obj)->GetPrintoutForPrinting();
}
	
EWXWEXPORT(void,wxPrintPreview_SetFrame)(void* _obj,void* frame)
{
	((wxPrintPreview*)_obj)->SetFrame((wxFrame*)frame);
}
	
EWXWEXPORT(void,wxPrintPreview_SetCanvas)(void* _obj,void* canvas)
{
	((wxPrintPreview*)_obj)->SetCanvas((wxPreviewCanvas*)canvas);
}
	
EWXWEXPORT(void*,wxPrintPreview_GetFrame)(void* _obj)
{
	return (void*)((wxPrintPreview*)_obj)->GetFrame();
}
	
EWXWEXPORT(void*,wxPrintPreview_GetCanvas)(void* _obj)
{
	return (void*)((wxPrintPreview*)_obj)->GetCanvas();
}
	
EWXWEXPORT(int,wxPrintPreview_PaintPage)(wxPrintPreview* _obj,wxPreviewCanvas* canvas,wxDC* dc)
{
	return (int)_obj->PaintPage(canvas, *dc);
}
	
EWXWEXPORT(int,wxPrintPreview_DrawBlankPage)(wxPrintPreview* _obj,wxPreviewCanvas* canvas,wxDC* dc)
{
	return (int)_obj->DrawBlankPage(canvas, *dc);
}
	
EWXWEXPORT(int,wxPrintPreview_RenderPage)(wxPrintPreview* _obj,int pageNum)
{
	return (int)_obj->RenderPage(pageNum);
}
	
EWXWEXPORT(void,wxPrintPreview_GetPrintDialogData)(void* _obj,void* _ref)
{
	*((wxPrintDialogData*)_ref) = ((wxPrintPreview*)_obj)->GetPrintDialogData();
}
	
EWXWEXPORT(void,wxPrintPreview_SetZoom)(void* _obj,int percent)
{
	((wxPrintPreview*)_obj)->SetZoom(percent);
}
	
EWXWEXPORT(int,wxPrintPreview_GetZoom)(void* _obj)
{
	return ((wxPrintPreview*)_obj)->GetZoom();
}
	
EWXWEXPORT(int,wxPrintPreview_GetMaxPage)(void* _obj)
{
	return ((wxPrintPreview*)_obj)->GetMaxPage();
}
	
EWXWEXPORT(int,wxPrintPreview_GetMinPage)(void* _obj)
{
	return ((wxPrintPreview*)_obj)->GetMinPage();
}
	
EWXWEXPORT(int,wxPrintPreview_IsOk)(wxPrintPreview* _obj)
{
	return (int)_obj->IsOk();
}
	
EWXWEXPORT(void,wxPrintPreview_SetOk)(void* _obj,int ok)
{
	((wxPrintPreview*)_obj)->SetOk(ok != 0);
}
	
EWXWEXPORT(int,wxPrintPreview_Print)(wxPrintPreview* _obj,int interactive)
{
	return (int)_obj->Print(interactive != 0);
}
	
EWXWEXPORT(void,wxPrintPreview_DetermineScaling)(void* _obj)
{
	((wxPrintPreview*)_obj)->DetermineScaling();
}
	
EWXWEXPORT(void*,wxPrintData_Create)()
{
	return (void*) new wxPrintData();
}

EWXWEXPORT(void,wxPrintData_Delete)(void* _obj)
{
    delete (wxPrintData*)_obj;
}

EWXWEXPORT(void*,wxPostScriptPrintNativeData_Create)()
{
#ifdef wxUSE_POSTSCRIPT
	return (void*) new wxPostScriptPrintNativeData();
#else
	return NULL;
#endif
}

EWXWEXPORT(void,wxPostScriptPrintNativeData_Delete)(void* _obj)
{
#ifdef wxUSE_POSTSCRIPT
    delete (wxPostScriptPrintNativeData*)_obj;
#endif
}

EWXWEXPORT(int,wxPrintData_GetNoCopies)(void* _obj)
{
	return ((wxPrintData*)_obj)->GetNoCopies();
}
	
EWXWEXPORT(int,wxPrintData_GetCollate)(wxPrintData* _obj)
{
	return (int)_obj->GetCollate();
}
	
EWXWEXPORT(int,wxPrintData_GetOrientation)(void* _obj)
{
	return ((wxPrintData*)_obj)->GetOrientation();
}
	
EWXWEXPORT(wxString*,wxPrintData_GetPrinterName)(void* _obj)
{
	return new wxString(((wxPrintData*)_obj)->GetPrinterName());
}
	
EWXWEXPORT(int,wxPrintData_GetColour)(wxPrintData* _obj)
{
	return (int)_obj->GetColour();
}
	
EWXWEXPORT(int,wxPrintData_GetDuplex)(wxPrintData* _obj)
{
	return (int)_obj->GetDuplex();
}
	
EWXWEXPORT(int,wxPrintData_GetPaperId)(wxPrintData* _obj)
{
	return (int)_obj->GetPaperId();
}
	
EWXWEXPORT(void, wxPrintData_GetPaperSize)(void* _obj, void* w, void* h)
{
	wxSize tmp = ((wxPrintData*)_obj)->GetPaperSize();
	*((int*)w) = tmp.x;
	*((int*)h) = tmp.y;
}
	
EWXWEXPORT(int,wxPrintData_GetQuality)(wxPrintData* _obj)
{
	return (int)_obj->GetQuality();
}
	
EWXWEXPORT(void,wxPrintData_SetNoCopies)(void* _obj,int v)
{
	((wxPrintData*)_obj)->SetNoCopies(v);
}
	
EWXWEXPORT(void,wxPrintData_SetCollate)(void* _obj,int flag)
{
	((wxPrintData*)_obj)->SetCollate(flag != 0);
}
	
EWXWEXPORT(void,wxPrintData_SetOrientation)(void* _obj,int orient)
{
	((wxPrintData*)_obj)->SetOrientation(orient);
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterName)(void* _obj,wxString* name)
{
	((wxPrintData*)_obj)->SetPrinterName(*name);
}
	
EWXWEXPORT(void,wxPrintData_SetColour)(void* _obj,int colour)
{
	((wxPrintData*)_obj)->SetColour(colour != 0);
}
	
EWXWEXPORT(void,wxPrintData_SetDuplex)(void* _obj,int duplex)
{
	((wxPrintData*)_obj)->SetDuplex((wxDuplexMode)duplex);
}
	
EWXWEXPORT(void,wxPrintData_SetPaperId)(void* _obj,int sizeId)
{
	((wxPrintData*)_obj)->SetPaperId((wxPaperSize)sizeId);
}
	
EWXWEXPORT(void,wxPrintData_SetPaperSize)(void* _obj,int w,int h)
{
	((wxPrintData*)_obj)->SetPaperSize(wxSize(w, h));
}
	
EWXWEXPORT(void,wxPrintData_SetQuality)(void* _obj,int quality)
{
	((wxPrintData*)_obj)->SetQuality((wxPrintQuality)quality);
}
	
EWXWEXPORT(wxString*,wxPrintData_GetPrinterCommand)(void* _obj)
{
#if wxVERSION_NUMBER < 2600 || defined (wxUSE_POSTSCRIPT)
#ifdef wxUSE_POSTSCRIPT
	wxString tmp = ((wxPostScriptPrintNativeData*)_obj)->GetPrinterCommand();
#else
	wxString tmp = ((wxPrintData*)_obj)->GetPrinterCommand();
#endif
	return new wxString(tmp);
#else
	return false;
#endif
}
	
EWXWEXPORT(wxString*,wxPrintData_GetPrinterOptions)(void* _obj)
{
#if wxVERSION_NUMBER < 2600 || defined (wxUSE_POSTSCRIPT)
#ifdef wxUSE_POSTSCRIPT
	wxString tmp = ((wxPostScriptPrintNativeData*)_obj)->GetPrinterOptions();
#else
	wxString tmp = ((wxPrintData*)_obj)->GetPrinterOptions();
#endif
	return new wxString(tmp);
#else
	return false;
#endif
}
	
EWXWEXPORT(wxString*,wxPrintData_GetPreviewCommand)(void* _obj)
{
#if wxVERSION_NUMBER < 2600 || defined (wxUSE_POSTSCRIPT)
#ifdef wxUSE_POSTSCRIPT
	wxString tmp = ((wxPostScriptPrintNativeData*)_obj)->GetPreviewCommand();
#else
	wxString tmp = ((wxPrintData*)_obj)->GetPreviewCommand();
#endif
	return new wxString(tmp);
#else
	return false;
#endif
}
	
EWXWEXPORT(wxString*,wxPrintData_GetFilename)(void* _obj)
{
	wxString tmp = ((wxPrintData*)_obj)->GetFilename();
	return new wxString(tmp);
}
	
EWXWEXPORT(wxString*,wxPrintData_GetFontMetricPath)(void* _obj)
{
#if wxVERSION_NUMBER < 2600 || defined (wxUSE_POSTSCRIPT)
#ifdef wxUSE_POSTSCRIPT
	wxString tmp = ((wxPostScriptPrintNativeData*)_obj)->GetFontMetricPath();
#else
	wxString tmp = ((wxPrintData*)_obj)->GetFontMetricPath();
#endif
	return new wxString(tmp);
#else
	return false;
#endif
}
	
EWXWEXPORT(double,wxPrintData_GetPrinterScaleX)(void* _obj)
{
#ifdef wxUSE_POSTSCRIPT
	return ((wxPostScriptPrintNativeData*)_obj)->GetPrinterScaleX();
#elif wxVERSION_NUMBER < 2600
	return ((wxPrintData*)_obj)->GetPrinterScaleX();
#else
	return false;
#endif
}
	
EWXWEXPORT(double,wxPrintData_GetPrinterScaleY)(void* _obj)
{
#ifdef wxUSE_POSTSCRIPT
	return ((wxPostScriptPrintNativeData*)_obj)->GetPrinterScaleY();
#elif wxVERSION_NUMBER < 2600
	return ((wxPrintData*)_obj)->GetPrinterScaleY();
#else
	return false;
#endif
}
	
EWXWEXPORT(int,wxPrintData_GetPrinterTranslateX)(void* _obj)
{
#ifdef wxUSE_POSTSCRIPT
	return ((wxPostScriptPrintNativeData*)_obj)->GetPrinterTranslateX();
#elif wxVERSION_NUMBER < 2600
	return ((wxPrintData*)_obj)->GetPrinterTranslateX();
#else
	return false;
#endif
}
	
EWXWEXPORT(int,wxPrintData_GetPrinterTranslateY)(void* _obj)
{
#ifdef wxUSE_POSTSCRIPT
	return ((wxPostScriptPrintNativeData*)_obj)->GetPrinterTranslateY();
#elif wxVERSION_NUMBER < 2600
	return ((wxPrintData*)_obj)->GetPrinterTranslateY();
#else
	return false;
#endif
}
	
EWXWEXPORT(int,wxPrintData_GetPrintMode)(wxPrintData* _obj)
{
	return (int)_obj->GetPrintMode();
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterCommand)(void* _obj,wxString* command)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetPrinterCommand(*command);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetPrinterCommand(*command);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterOptions)(void* _obj,wxString* options)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetPrinterOptions(*options);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetPrinterOptions(*options);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPreviewCommand)(void* _obj,wxString* command)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetPreviewCommand(*command);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetPreviewCommand(*command);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetFilename)(void* _obj,wxString* filename)
{
	((wxPrintData*)_obj)->SetFilename(*filename);
}
	
EWXWEXPORT(void,wxPrintData_SetFontMetricPath)(void* _obj,wxString* path)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetFontMetricPath(*path);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetFontMetricPath(*path);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterScaleX)(void* _obj,double x)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetPrinterScaleX(x);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetPrinterScaleX(x);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterScaleY)(void* _obj,double y)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetPrinterScaleY(y);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetPrinterScaleY(y);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterScaling)(void* _obj,double x,double y)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetPrinterScaling(x, y);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetPrinterScaling(x, y);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterTranslateX)(void* _obj,int x)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetPrinterTranslateX((int)x);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetPrinterTranslateX((int)x);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterTranslateY)(void* _obj,int y)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetPrinterTranslateY((int)y);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetPrinterTranslateY((long)y);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterTranslation)(void* _obj,int x,int y)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)_obj)->SetPrinterTranslation((long)x, (long)y);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)_obj)->SetPrinterTranslation((long)x, (long)y);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrintMode)(void* _obj,int printMode)
{
	((wxPrintData*)_obj)->SetPrintMode((wxPrintMode)printMode);
}
	
EWXWEXPORT(void,wxPrintData_Assign)(void* _obj,void* data)
{
	*((wxPrintData*)_obj) = *((wxPrintData*)data);
}
	
EWXWEXPORT(void*,wxPrintDialogData_CreateDefault)()
{
	return (void*) new wxPrintDialogData();
}

EWXWEXPORT(void*,wxPrintDialogData_CreateFromData)(void* printData)
{
	return (void*) new wxPrintDialogData(*((wxPrintData*)printData));
}

EWXWEXPORT(void,wxPrintDialogData_Delete)(void* _obj)
{
	delete (wxPrintDialogData*)_obj;
}

EWXWEXPORT(int,wxPrintDialogData_GetFromPage)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetFromPage();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetToPage)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetToPage();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetMinPage)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetMinPage();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetMaxPage)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetMaxPage();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetNoCopies)(void* _obj)
{
	return ((wxPrintDialogData*)_obj)->GetNoCopies();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetAllPages)(wxPrintDialogData* _obj)
{
	return (int)_obj->GetAllPages();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetSelection)(wxPrintDialogData* _obj)
{
	return (int)_obj->GetSelection();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetCollate)(wxPrintDialogData* _obj)
{
	return (int)_obj->GetCollate();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetPrintToFile)(wxPrintDialogData* _obj)
{
	return (int)_obj->GetPrintToFile();
}

EWXWEXPORT(void,wxPrintDialogData_SetFromPage)(void* _obj,int v)
{
	((wxPrintDialogData*)_obj)->SetFromPage(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetToPage)(void* _obj,int v)
{
	((wxPrintDialogData*)_obj)->SetToPage(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetMinPage)(void* _obj,int v)
{
	((wxPrintDialogData*)_obj)->SetMinPage(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetMaxPage)(void* _obj,int v)
{
	((wxPrintDialogData*)_obj)->SetMaxPage(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetNoCopies)(void* _obj,int v)
{
	((wxPrintDialogData*)_obj)->SetNoCopies(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetAllPages)(void* _obj,int flag)
{
	((wxPrintDialogData*)_obj)->SetAllPages(flag != 0);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetSelection)(void* _obj,int flag)
{
	((wxPrintDialogData*)_obj)->SetSelection(flag != 0);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetCollate)(void* _obj,int flag)
{
	((wxPrintDialogData*)_obj)->SetCollate(flag != 0);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetPrintToFile)(void* _obj,int flag)
{
	((wxPrintDialogData*)_obj)->SetPrintToFile(flag != 0);
}

EWXWEXPORT(void,wxPrintDialogData_EnablePrintToFile)(void* _obj,int flag)
{
	((wxPrintDialogData*)_obj)->EnablePrintToFile(flag != 0);
}
	
EWXWEXPORT(void,wxPrintDialogData_EnableSelection)(void* _obj,int flag)
{
	((wxPrintDialogData*)_obj)->EnableSelection(flag != 0);
}
	
EWXWEXPORT(void,wxPrintDialogData_EnablePageNumbers)(void* _obj,int flag)
{
	((wxPrintDialogData*)_obj)->EnablePageNumbers(flag != 0);
}
	
EWXWEXPORT(void,wxPrintDialogData_EnableHelp)(void* _obj,int flag)
{
	((wxPrintDialogData*)_obj)->EnableHelp(flag != 0);
}
	
EWXWEXPORT(int,wxPrintDialogData_GetEnablePrintToFile)(wxPrintDialogData* _obj)
{
	return (int)_obj->GetEnablePrintToFile();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetEnableSelection)(wxPrintDialogData* _obj)
{
	return (int)_obj->GetEnableSelection();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetEnablePageNumbers)(wxPrintDialogData* _obj)
{
	return (int)_obj->GetEnablePageNumbers();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetEnableHelp)(wxPrintDialogData* _obj)
{
	return (int)_obj->GetEnableHelp();
}
	
EWXWEXPORT(void,wxPrintDialogData_GetPrintData)(void* _obj,void* _ref)
{
	*((wxPrintData*)_ref) = ((wxPrintDialogData*)_obj)->GetPrintData();
}
	
EWXWEXPORT(void,wxPrintDialogData_SetPrintData)(void* _obj,void* printData)
{
	((wxPrintDialogData*)_obj)->SetPrintData(*((wxPrintData*)printData));
}
	
EWXWEXPORT(void,wxPrintDialogData_Assign)(void* _obj,void* data)
{
	*((wxPrintDialogData*)_obj) = *((wxPrintDialogData*)data);
}
	
EWXWEXPORT(void,wxPrintDialogData_AssignData)(void* _obj,void* data)
{
	*((wxPrintDialogData*)_obj) = *((wxPrintData*)data);
}
	
EWXWEXPORT(wxPostScriptDC*,wxPostScriptDC_Create)(wxPrintData* printData)
{
#ifdef wxUSE_POSTSCRIPT
	return new wxPostScriptDC(*printData);
#else
	return NULL;
#endif
}

EWXWEXPORT(void,wxPostScriptDC_Delete)(wxPostScriptDC* self)
{
#ifdef wxUSE_POSTSCRIPT
	if (self) delete self;
#endif
}

EWXWEXPORT(void,wxPostScriptDC_SetResolution)(wxPostScriptDC* self,int ppi)
{
#ifdef wxUSE_POSTSCRIPT
	self->SetResolution(ppi);
#endif
}

EWXWEXPORT(int,wxPostScriptDC_GetResolution)(wxPostScriptDC* self,int ppi)
{
#ifdef wxUSE_POSTSCRIPT
	return self->GetResolution();
#else
	return 0;
#endif
}

EWXWEXPORT(void*,wxPrinterDC_Create)(wxPrintData* printData)
{
#if defined(__WXGTK__) 
	return NULL;
#else
	return new wxPrinterDC(*printData);
#endif
}

EWXWEXPORT(void,wxPrinterDC_Delete)(void* _obj)
{
#if !defined(__WXGTK__)
	delete (wxPrinterDC*)_obj;
#endif
}

EWXWEXPORT(void, wxPrinterDC_GetPaperRect) (void* _obj, int* x, int* y, int* w, int* h)
{
#if !defined(__WXGTK__)
	wxRect rct = ((wxPrinterDC*)_obj)->GetPaperRect();
	*x = rct.x;
	*y = rct.y;
	*w = rct.width;
	*h = rct.height;
#endif
}

}
