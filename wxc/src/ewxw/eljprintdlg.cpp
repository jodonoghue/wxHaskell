#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxPrintDialog_Create) (void* parent, void* data)
{
    return (void*) new wxPrintDialog((wxWindow*)parent, (wxPrintData*)data);
}

EWXWEXPORT(void, wxPrintDialog_GetPrintData)(void* _obj, void* _ref)
{
	*((wxPrintData*)_ref) = ((wxPrintDialog*)_obj)->GetPrintData();
}
	
EWXWEXPORT(void*, wxPrintDialog_GetPrintDC)(void* _obj)
{
	return (void*)((wxPrintDialog*)_obj)->GetPrintDC();
}
	
EWXWEXPORT(void*, wxPageSetupDialog_Create) (void* parent, void* data)
{
    return (void*) new wxPageSetupDialog((wxWindow*)parent, (wxPageSetupData*)data);
}

EWXWEXPORT(void, wxPageSetupDialog_GetPageSetupData)(void* _obj, void* _ref)
{
	*((wxPageSetupData*)_ref) = ((wxPageSetupDialog*)_obj)->GetPageSetupData();
}
	
EWXWEXPORT(void*, wxPageSetupDialogData_Create)()
{
    return (void*) new wxPageSetupDialogData();
}

EWXWEXPORT(void*, wxPageSetupDialogData_CreateFromData)(void* printData)
{
    return (void*) new wxPageSetupDialogData(*((wxPrintData*)printData));
}

EWXWEXPORT(void, wxPageSetupDialogData_Delete)(void* _obj)
{
    delete (wxPageSetupDialogData*)_obj;
}

EWXWEXPORT(void, wxPageSetupDialogData_GetPaperSize)(void* _obj, void* w, void* h)
{
	wxSize tmp = ((wxPageSetupDialogData*)_obj)->GetPaperSize();
	*(int*)w = tmp.x;
	*(int*)h = tmp.y;
}
	
EWXWEXPORT(int, wxPageSetupDialogData_GetPaperId)(void* _obj)
{
	return (int)((wxPageSetupDialogData*)_obj)->GetPaperId();
}
	
EWXWEXPORT(void, wxPageSetupDialogData_GetMinMarginTopLeft)(void* _obj, void* x, void* y)
{
	wxPoint tmp = ((wxPageSetupDialogData*)_obj)->GetMinMarginTopLeft();
	*(int*)x = tmp.x;
	*(int*)y = tmp.y;
}
	
EWXWEXPORT(void, wxPageSetupDialogData_GetMinMarginBottomRight)(void* _obj, void* x, void* y)
{
	wxPoint tmp = ((wxPageSetupDialogData*)_obj)->GetMinMarginBottomRight();
	*(int*)x = tmp.x;
	*(int*)y = tmp.y;
}
	
EWXWEXPORT(void, wxPageSetupDialogData_GetMarginTopLeft)(void* _obj, void* x, void* y)
{
	wxPoint tmp = ((wxPageSetupDialogData*)_obj)->GetMarginTopLeft();
	*(int*)x = tmp.x;
	*(int*)y = tmp.y;
}
	
EWXWEXPORT(void, wxPageSetupDialogData_GetMarginBottomRight)(void* _obj, void* x, void* y)
{
	wxPoint tmp = ((wxPageSetupDialogData*)_obj)->GetMarginBottomRight();
	*(int*)x = tmp.x;
	*(int*)y = tmp.y;
}
	
EWXWEXPORT(int, wxPageSetupDialogData_GetDefaultMinMargins)(void* _obj)
{
	return (int)((wxPageSetupDialogData*)_obj)->GetDefaultMinMargins();
}
	
EWXWEXPORT(int, wxPageSetupDialogData_GetEnableMargins)(void* _obj)
{
	return (int)((wxPageSetupDialogData*)_obj)->GetEnableMargins();
}
	
EWXWEXPORT(int, wxPageSetupDialogData_GetEnableOrientation)(void* _obj)
{
	return (int)((wxPageSetupDialogData*)_obj)->GetEnableOrientation();
}
	
EWXWEXPORT(int, wxPageSetupDialogData_GetEnablePaper)(void* _obj)
{
	return (int)((wxPageSetupDialogData*)_obj)->GetEnablePaper();
}
	
EWXWEXPORT(int, wxPageSetupDialogData_GetEnablePrinter)(void* _obj)
{
	return (int)((wxPageSetupDialogData*)_obj)->GetEnablePrinter();
}
	
EWXWEXPORT(int, wxPageSetupDialogData_GetDefaultInfo)(void* _obj)
{
	return (int)((wxPageSetupDialogData*)_obj)->GetDefaultInfo();
}
	
EWXWEXPORT(int, wxPageSetupDialogData_GetEnableHelp)(void* _obj)
{
	return (int)((wxPageSetupDialogData*)_obj)->GetEnableHelp();
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetPaperSize)(void* _obj, int w, int h)
{
	((wxPageSetupDialogData*)_obj)->SetPaperSize(wxSize(w, h));
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetPaperId)(void* _obj, void* id)
{
	((wxPageSetupDialogData*)_obj)->SetPaperId(*((wxPaperSize*)id));
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetPaperSizeId)(void* _obj, int id)
{
	((wxPageSetupDialogData*)_obj)->SetPaperSize((wxPaperSize)id);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetMinMarginTopLeft)(void* _obj, int x, int y)
{
	((wxPageSetupDialogData*)_obj)->SetMinMarginTopLeft(wxPoint(x, y));
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetMinMarginBottomRight)(void* _obj, int x, int y)
{
	((wxPageSetupDialogData*)_obj)->SetMinMarginBottomRight(wxPoint(x, y));
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetMarginTopLeft)(void* _obj, int x, int y)
{
	((wxPageSetupDialogData*)_obj)->SetMarginTopLeft(wxPoint(x, y));
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetMarginBottomRight)(void* _obj, int x, int y)
{
	((wxPageSetupDialogData*)_obj)->SetMarginBottomRight(wxPoint(x, y));
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetDefaultMinMargins)(void* _obj, int flag)
{
	((wxPageSetupDialogData*)_obj)->SetDefaultMinMargins(flag != 0);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetDefaultInfo)(void* _obj, int flag)
{
	((wxPageSetupDialogData*)_obj)->SetDefaultInfo(flag != 0);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_EnableMargins)(void* _obj, int flag)
{
	((wxPageSetupDialogData*)_obj)->EnableMargins(flag != 0);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_EnableOrientation)(void* _obj, int flag)
{
	((wxPageSetupDialogData*)_obj)->EnableOrientation(flag != 0);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_EnablePaper)(void* _obj, int flag)
{
	((wxPageSetupDialogData*)_obj)->EnablePaper(flag != 0);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_EnablePrinter)(void* _obj, int flag)
{
	((wxPageSetupDialogData*)_obj)->EnablePrinter(flag != 0);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_EnableHelp)(void* _obj, int flag)
{
	((wxPageSetupDialogData*)_obj)->EnableHelp(flag != 0);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_CalculateIdFromPaperSize)(void* _obj)
{
	((wxPageSetupDialogData*)_obj)->CalculateIdFromPaperSize();
}
	
EWXWEXPORT(void, wxPageSetupDialogData_CalculatePaperSizeFromId)(void* _obj)
{
	((wxPageSetupDialogData*)_obj)->CalculatePaperSizeFromId();
}
	
EWXWEXPORT(void, wxPageSetupDialogData_Assign)(void* _obj, void* data)
{
	*((wxPageSetupDialogData*)_obj) = *((wxPageSetupDialogData*)data);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_AssignData)(void* _obj, void* data)
{
	*((wxPageSetupDialogData*)_obj) = *((wxPrintData*)data);
}
	
EWXWEXPORT(void, wxPageSetupDialogData_GetPrintData)(void* _obj, void* _ref)
{
	*((wxPrintData*)_ref) = ((wxPageSetupDialogData*)_obj)->GetPrintData();
}
	
EWXWEXPORT(void, wxPageSetupDialogData_SetPrintData)(void* _obj, void* printData)
{
	((wxPageSetupDialogData*)_obj)->SetPrintData(*((wxPrintData*)printData));
}
	
}
