#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxNotebook_Create) (void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxNotebook ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int, wxNotebook_GetPageCount)(void* _obj)
{
	return ((wxNotebook*)_obj)->GetPageCount();
}
	
EWXWEXPORT(int, wxNotebook_SetSelection)(void* _obj, int nPage)
{
	return ((wxNotebook*)_obj)->SetSelection(nPage);
}
	
EWXWEXPORT(void, wxNotebook_AdvanceSelection)(void* _obj, int bForward)
{
	((wxNotebook*)_obj)->AdvanceSelection(bForward != 0);
}
	
EWXWEXPORT(int, wxNotebook_GetSelection)(void* _obj)
{
	return ((wxNotebook*)_obj)->GetSelection();
}
	
EWXWEXPORT(int, wxNotebook_SetPageText)(void* _obj, int nPage, void* strText)
{
	return (int)((wxNotebook*)_obj)->SetPageText(nPage, (char*) strText);
}
	
EWXWEXPORT(int, wxNotebook_GetPageText)(void* _obj, int nPage, void* _buf)
{
	wxString result = ((wxNotebook*)_obj)->GetPageText(nPage);
	if (_buf) memcpy (_buf, result.c_str(), result.Length());
	return result.Length();
}
	
EWXWEXPORT(void, wxNotebook_SetImageList)(void* _obj, void* imageList)
{
	((wxNotebook*)_obj)->SetImageList((wxImageList*) imageList);
}
	
EWXWEXPORT(void*, wxNotebook_GetImageList)(void* _obj)
{
	return (void*)((wxNotebook*)_obj)->GetImageList();
}
	
EWXWEXPORT(int, wxNotebook_GetPageImage)(void* _obj, int nPage)
{
	return ((wxNotebook*)_obj)->GetPageImage(nPage);
}
	
EWXWEXPORT(int, wxNotebook_SetPageImage)(void* _obj, int nPage, int nImage)
{
	return (int)((wxNotebook*)_obj)->SetPageImage(nPage, nImage);
}
	
EWXWEXPORT(int, wxNotebook_GetRowCount)(void* _obj)
{
	return ((wxNotebook*)_obj)->GetRowCount();
}
	
EWXWEXPORT(void, wxNotebook_SetPageSize)(void* _obj, int _w, int _h)
{
	((wxNotebook*)_obj)->SetPageSize(wxSize(_w, _h));
}
	
EWXWEXPORT(void, wxNotebook_SetPadding)(void* _obj, int _w, int _h)
{
	((wxNotebook*)_obj)->SetPadding(wxSize(_w, _h));
}
	
EWXWEXPORT(int, wxNotebook_DeletePage)(void* _obj, int nPage)
{
	return (int)((wxNotebook*)_obj)->DeletePage(nPage);
}
	
EWXWEXPORT(int, wxNotebook_RemovePage)(void* _obj, int nPage)
{
	return (int)((wxNotebook*)_obj)->RemovePage(nPage);
}
	
EWXWEXPORT(int, wxNotebook_DeleteAllPages)(void* _obj)
{
	return (int)((wxNotebook*)_obj)->DeleteAllPages();
}
	
EWXWEXPORT(int, wxNotebook_AddPage)(void* _obj, void* pPage, void* strText, int bSelect, int imageId)
{
	return (int)((wxNotebook*)_obj)->AddPage((wxNotebookPage*) pPage, (char*) strText, bSelect != 0, imageId);
}
	
EWXWEXPORT(int, wxNotebook_InsertPage)(void* _obj, int nPage, void* pPage, void* strText, int bSelect, int imageId)
{
	return (int)((wxNotebook*)_obj)->InsertPage(nPage, (wxNotebookPage*) pPage, (char*) strText, bSelect != 0, imageId);
}
	
EWXWEXPORT(void*, wxNotebook_GetPage)(void* _obj, int nPage)
{
	return (void*)((wxNotebook*)_obj)->GetPage(nPage);
}
	
}
