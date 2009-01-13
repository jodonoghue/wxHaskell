#include "wrapper.h"
#include "wx/fs_zip.h"

extern "C"
{

EWXWEXPORT(void*,wxHtmlHelpController_Create)(int _style)
{
	wxGetApp().InitZipFileSystem();
	wxGetApp().InitImageHandlers();
	return (void*) new wxHtmlHelpController(_style);
}

EWXWEXPORT(void,wxHtmlHelpController_Delete)(void* _obj)
{
	delete (wxHtmlHelpController*)_obj;
}

EWXWEXPORT(void,wxHtmlHelpController_SetTitleFormat)(void* _obj,wxString* format)
{
	((wxHtmlHelpController*)_obj)->SetTitleFormat(*format);
}
	
EWXWEXPORT(void,wxHtmlHelpController_SetTempDir)(void* _obj,wxString* path)
{
	((wxHtmlHelpController*)_obj)->SetTempDir(*path);
}
	
EWXWEXPORT(int,wxHtmlHelpController_AddBook)(wxHtmlHelpController* _obj,wxString* book,int show_wait_msg)
{
	return (int)_obj->AddBook(* book, show_wait_msg != 0);
}
	
EWXWEXPORT(void,wxHtmlHelpController_Display)(wxHtmlHelpController* _obj,wxString* x)
{
	_obj->Display(*x);
}
	
EWXWEXPORT(void,wxHtmlHelpController_DisplayNumber)(wxHtmlHelpController* _obj,int id)
{
	_obj->Display(id);
}
	
EWXWEXPORT(void,wxHtmlHelpController_DisplayContents)(wxHtmlHelpController* _obj)
{
	_obj->DisplayContents();
}
	
EWXWEXPORT(void,wxHtmlHelpController_DisplayIndex)(wxHtmlHelpController* _obj)
{
	_obj->DisplayIndex();
}
	
EWXWEXPORT(int,wxHtmlHelpController_KeywordSearch)(wxHtmlHelpController* _obj,wxString* keyword)
{
	return (int)_obj->KeywordSearch(*keyword);
}
	
EWXWEXPORT(void*,wxHtmlHelpController_GetFrame)(void* _obj)
{
	return (void*)((wxHtmlHelpController*)_obj)->GetFrame();
}
	
EWXWEXPORT(void,wxHtmlHelpController_UseConfig)(void* _obj,void* config,wxString* rootpath)
{
	((wxHtmlHelpController*)_obj)->UseConfig((wxConfigBase*)config, *rootpath);
}
	
EWXWEXPORT(void,wxHtmlHelpController_ReadCustomization)(void* _obj,void* cfg,wxString* path)
{
	((wxHtmlHelpController*)_obj)->ReadCustomization((wxConfigBase*)cfg, *path);
}
	
EWXWEXPORT(void,wxHtmlHelpController_WriteCustomization)(void* _obj,void* cfg,wxString* path)
{
	((wxHtmlHelpController*)_obj)->WriteCustomization((wxConfigBase*)cfg, *path);
}
	
EWXWEXPORT(int,wxHtmlHelpController_Initialize)(wxHtmlHelpController* _obj,wxString* file)
{
	return (int)_obj->Initialize(*file);
}
	
EWXWEXPORT(void,wxHtmlHelpController_SetViewer)(void* _obj,wxString* viewer,int flags)
{
	((wxHtmlHelpController*)_obj)->SetViewer(*viewer, (long)flags);
}
	
EWXWEXPORT(int,wxHtmlHelpController_LoadFile)(wxHtmlHelpController* _obj,wxString* file)
{
	return (int)_obj->LoadFile(* file);
}
	
EWXWEXPORT(int,wxHtmlHelpController_DisplaySectionNumber)(wxHtmlHelpController* _obj,int sectionNo)
{
	return (int)_obj->DisplaySection(sectionNo);
}
	
EWXWEXPORT(int,wxHtmlHelpController_DisplaySection)(wxHtmlHelpController* _obj,wxString* section)
{
	return (int)_obj->DisplaySection(*section);
}
	
EWXWEXPORT(int,wxHtmlHelpController_DisplayBlock)(wxHtmlHelpController* _obj,int blockNo)
{
	return (int)_obj->DisplayBlock((long)blockNo);
}
	
EWXWEXPORT(void,wxHtmlHelpController_SetFrameParameters)(void* _obj,wxString* title,int width,int height,int pos_x,int pos_y,int newFrameEachTime)
{
	((wxHtmlHelpController*)_obj)->SetFrameParameters(* title, wxSize(width, height), wxPoint(pos_x, pos_y), newFrameEachTime != 0);
}
	
EWXWEXPORT(void*,wxHtmlHelpController_GetFrameParameters)(void* _obj,void* title,int* width,int* height,int* pos_x,int* pos_y,int* newFrameEachTime)
{
	void* result;
	wxPoint pos;
	wxSize size;

	result = (void*)((wxHtmlHelpController*)_obj)->GetFrameParameters(&size, &pos, (bool*)newFrameEachTime);
	
	*height = size.y;
	*width  = size.x;
	*pos_x  = pos.x;
	*pos_y  = pos.y;
	
	return result;
}
	
EWXWEXPORT(int,wxHtmlHelpController_Quit)(wxHtmlHelpController* _obj)
{
	return (int)_obj->Quit();
}
	
}
