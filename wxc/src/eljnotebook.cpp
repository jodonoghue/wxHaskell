#include "wrapper.h"

extern "C"
{

EWXWEXPORT(int,expNB_TOP)()
{
    return (int)wxNB_TOP;
}

EWXWEXPORT(int,expNB_BOTTOM)()
{
    return (int)wxNB_BOTTOM;
}

EWXWEXPORT(int,expNB_LEFT)()
{
    return (int)wxNB_LEFT;
}

EWXWEXPORT(int,expNB_RIGHT)()
{
    return (int)wxNB_RIGHT;
}

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
	return (int)((wxNotebook*)_obj)->SetPageText(nPage, (wxChar*) strText);
}
	
EWXWEXPORT(int, wxNotebook_GetPageText)(void* _obj, int nPage, void* _buf)
{
	wxString result = ((wxNotebook*)_obj)->GetPageText(nPage);
	return copyStrToBuf(_buf, result);
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

EWXWEXPORT(int, wxNotebook_HitTest)(wxNotebook* _obj, int x, int y, long *flags)
{
	return _obj->HitTest(wxPoint(x, y), flags);
}

EWXWEXPORT(int,expBK_HITTEST_NOWHERE)()
{
    return (int)wxBK_HITTEST_NOWHERE;
}

EWXWEXPORT(int,expBK_HITTEST_ONICON)()
{
    return (int)wxBK_HITTEST_ONICON;
}

EWXWEXPORT(int,expBK_HITTEST_ONLABEL)()
{
    return (int)wxBK_HITTEST_ONLABEL;
}

EWXWEXPORT(int,expBK_HITTEST_ONITEM)()
{
    return (int)wxBK_HITTEST_ONITEM;
}

EWXWEXPORT(int,expBK_HITTEST_ONPAGE)()
{
    return (int)wxBK_HITTEST_ONPAGE;
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
	return (int)((wxNotebook*)_obj)->AddPage((wxNotebookPage*) pPage, (wxChar*) strText, bSelect != 0, imageId);
}
	
EWXWEXPORT(int, wxNotebook_InsertPage)(void* _obj, int nPage, void* pPage, void* strText, int bSelect, int imageId)
{
	return (int)((wxNotebook*)_obj)->InsertPage(nPage, (wxNotebookPage*) pPage, (wxChar*) strText, bSelect != 0, imageId);
}
	
EWXWEXPORT(void*, wxNotebook_GetPage)(void* _obj, int nPage)
{
	return (void*)((wxNotebook*)_obj)->GetPage(nPage);
}
	
}
