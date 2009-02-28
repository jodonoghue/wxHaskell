#include "wrapper.h"

extern "C"
{

EWXWEXPORT(wxToolBar*,wxToolBar_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return new wxToolBar (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void,wxToolBar_Delete)(wxToolBar* self)
{
	delete self;
}

EWXWEXPORT(int,wxToolBar_AddControl)(wxToolBar* self,wxControl* ctrl)
{
	return self->AddControl(ctrl) != NULL;
}

EWXWEXPORT(void,wxToolBar_AddSeparator)(wxToolBar* self)
{
	self->AddSeparator ();
}

EWXWEXPORT(void,wxToolBar_AddTool)(wxToolBar* self,int id,wxBitmap* bmp,wxString* shelp,wxString* lhelp)
{
	self->AddTool (id,*bmp,*shelp,*lhelp);
}

EWXWEXPORT(void,wxToolBar_AddToolEx)(wxToolBar* self,int id,wxBitmap* bmp1,wxBitmap* bmp2,bool tgl,int x,int y,wxObject* dat,wxString* shelp,wxString* lhelp)
{
	self->AddTool (id,*bmp1,*bmp2, tgl, x, y, dat,*shelp,*lhelp);
}

EWXWEXPORT(bool,wxToolBar_DeleteTool)(wxToolBar* self,int id)
{
	return self->DeleteTool (id);
}

EWXWEXPORT(bool,wxToolBar_DeleteToolByPos)(wxToolBar* self,int pos)
{
	return self->DeleteToolByPos (pos);
}

EWXWEXPORT(void,wxToolBar_EnableTool)(wxToolBar* self,int id,bool enb)
{
	self->EnableTool (id, enb);
}

EWXWEXPORT(void, wxToolBar_GetToolSize) (void* _obj, void* x, void* y)
{
	*((int*)x) = ((wxToolBar*)_obj)->GetToolSize().x;
	*((int*)y) = ((wxToolBar*)_obj)->GetToolSize().y;
}

EWXWEXPORT(void, wxToolBar_GetToolBitmapSize) (void* _obj, void* x, void* y)
{
	*((int*)x) = ((wxToolBar*)_obj)->GetToolBitmapSize().x;
	*((int*)y) = ((wxToolBar*)_obj)->GetToolBitmapSize().y;
}

EWXWEXPORT(void, wxToolBar_GetMargins) (void* _obj, void* x, void* y)
{
	*((int*)x) = ((wxToolBar*)_obj)->GetMargins().x;
	*((int*)y) = ((wxToolBar*)_obj)->GetMargins().y;
}

EWXWEXPORT(void*,wxToolBar_GetToolClientData)(wxToolBar* self,int id)
{
	return (void*)self->GetToolClientData (id);
}

EWXWEXPORT(bool,wxToolBar_GetToolEnabled)(wxToolBar* self,int id)
{
	return self->GetToolEnabled (id);
}

EWXWEXPORT(wxString*,wxToolBar_GetToolLongHelp)(wxToolBar* self,int id)
{
	wxString *result = new wxString();
	*result = self->GetToolLongHelp (id);
	return result;
}

EWXWEXPORT(int,wxToolBar_GetToolPacking)(wxToolBar* self)
{
	return self->GetToolPacking ();
}

EWXWEXPORT(wxString*,wxToolBar_GetToolShortHelp)(wxToolBar* self,int id)
{
	wxString *result = new wxString();
	*result = self->GetToolShortHelp (id);
	return result;
}

EWXWEXPORT(bool,wxToolBar_GetToolState)(wxToolBar* self,int id)
{
	return self->GetToolState (id);
}

EWXWEXPORT(void,wxToolBar_InsertControl)(wxToolBar* self,int pos,wxControl* ctrl)
{
	self->InsertControl ((size_t)pos, ctrl);
}

EWXWEXPORT(void,wxToolBar_InsertSeparator)(wxToolBar* self,int pos)
{
	self->InsertSeparator ((size_t)pos);
}

EWXWEXPORT(void,wxToolBar_InsertTool)(wxToolBar* self,int pos,int id,wxBitmap* bmp1,wxBitmap* bmp2,bool tgl,wxObject* dat,wxString* shelp,wxString* lhelp)
{
	self->InsertTool ((size_t)pos, id,*bmp1,*bmp2, tgl, dat,*shelp,*lhelp);
}

EWXWEXPORT(bool,wxToolBar_Realize)(wxToolBar* self)
{
	return self->Realize ();
}

EWXWEXPORT(void,wxToolBar_RemoveTool)(wxToolBar* self,int id)
{
	self->RemoveTool (id);
}

EWXWEXPORT(void,wxToolBar_SetMargins)(wxToolBar* self,int x,int y)
{
#ifdef __WIN32__
	self->SetMargins(wxSize(x, y));
#else
	self->SetMargins(x, y);
#endif
}

EWXWEXPORT(void,wxToolBar_SetToolBitmapSize)(wxToolBar* self,int x,int y)
{
	self->SetToolBitmapSize (wxSize(x, y));
}

EWXWEXPORT(void,wxToolBar_SetToolClientData)(wxToolBar* self,int id,wxObject* dat)
{
	self->SetToolClientData (id, dat);
}

EWXWEXPORT(void,wxToolBar_SetToolLongHelp)(wxToolBar* self,int id,wxString* str)
{
	self->SetToolLongHelp (id,*str);
}

EWXWEXPORT(void,wxToolBar_SetToolPacking)(wxToolBar* self,int val)
{
	self->SetToolPacking (val);
}

EWXWEXPORT(void,wxToolBar_SetToolShortHelp)(wxToolBar* self,int id,wxString* str)
{
	self->SetToolShortHelp (id,*str);
}

EWXWEXPORT(void,wxToolBar_SetToolSeparation)(wxToolBar* self,int val)
{
	self->SetToolSeparation (val);
}

EWXWEXPORT(void,wxToolBar_ToggleTool)(wxToolBar* self,int id,bool val)
{
	self->ToggleTool (id, val);
}

}
