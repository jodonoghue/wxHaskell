#include "wrapper.h"
#include "wx/fs_zip.h"

extern "C"
{

EWXWEXPORT(void*, wxHtmlHelpController_Create)(int _style)
{
	wxGetApp().InitZipFileSystem();
	wxGetApp().InitImageHandlers();
	return (void*) new wxHtmlHelpController(_style);
}

EWXWEXPORT(void, wxHtmlHelpController_Delete)(void* _obj)
{
	delete (wxHtmlHelpController*)_obj;
}

EWXWEXPORT(void, wxHtmlHelpController_SetTitleFormat)(void* _obj, void* format)
{
	((wxHtmlHelpController*)_obj)->SetTitleFormat((char*)format);
}
	
EWXWEXPORT(void, wxHtmlHelpController_SetTempDir)(void* _obj, void* path)
{
	((wxHtmlHelpController*)_obj)->SetTempDir((char*)path);
}
	
EWXWEXPORT(int, wxHtmlHelpController_AddBook)(void* _obj, void* book, int show_wait_msg)
{
	return (int)((wxHtmlHelpController*)_obj)->AddBook((char*) book, show_wait_msg != 0);
}
	
EWXWEXPORT(int, wxHtmlHelpController_Display)(void* _obj, void* x)
{
	return (int)((wxHtmlHelpController*)_obj)->Display((char*)x);
}
	
EWXWEXPORT(int, wxHtmlHelpController_DisplayNumber)(void* _obj, int id)
{
	return (int)((wxHtmlHelpController*)_obj)->Display(id);
}
	
EWXWEXPORT(int, wxHtmlHelpController_DisplayContents)(void* _obj)
{
	return (int)((wxHtmlHelpController*)_obj)->DisplayContents();
}
	
EWXWEXPORT(int, wxHtmlHelpController_DisplayIndex)(void* _obj)
{
	return (int)((wxHtmlHelpController*)_obj)->DisplayIndex();
}
	
EWXWEXPORT(int, wxHtmlHelpController_KeywordSearch)(void* _obj, void* keyword)
{
	return (int)((wxHtmlHelpController*)_obj)->KeywordSearch((char*) keyword);
}
	
EWXWEXPORT(void*, wxHtmlHelpController_GetFrame)(void* _obj)
{
	return (void*)((wxHtmlHelpController*)_obj)->GetFrame();
}
	
EWXWEXPORT(void, wxHtmlHelpController_UseConfig)(void* _obj, void* config, void* rootpath)
{
	((wxHtmlHelpController*)_obj)->UseConfig((wxConfigBase*)config, (char*)rootpath);
}
	
EWXWEXPORT(void, wxHtmlHelpController_ReadCustomization)(void* _obj, void* cfg, void* path)
{
	((wxHtmlHelpController*)_obj)->ReadCustomization((wxConfigBase*)cfg, (char*)path);
}
	
EWXWEXPORT(void, wxHtmlHelpController_WriteCustomization)(void* _obj, void* cfg, void* path)
{
	((wxHtmlHelpController*)_obj)->WriteCustomization((wxConfigBase*)cfg, (char*)path);
}
	
EWXWEXPORT(int, wxHtmlHelpController_Initialize)(void* _obj, void* file)
{
	return (int)((wxHtmlHelpController*)_obj)->Initialize((char*)file);
}
	
EWXWEXPORT(void, wxHtmlHelpController_SetViewer)(void* _obj, void* viewer, int flags)
{
	((wxHtmlHelpController*)_obj)->SetViewer((char*)viewer, (long)flags);
}
	
EWXWEXPORT(int, wxHtmlHelpController_LoadFile)(void* _obj, void* file)
{
	return (int)((wxHtmlHelpController*)_obj)->LoadFile((char*) file);
}
	
EWXWEXPORT(int, wxHtmlHelpController_DisplaySectionNumber)(void* _obj, int sectionNo)
{
	return (int)((wxHtmlHelpController*)_obj)->DisplaySection(sectionNo);
}
	
EWXWEXPORT(int, wxHtmlHelpController_DisplaySection)(void* _obj, void* section)
{
	return (int)((wxHtmlHelpController*)_obj)->DisplaySection((char*)section);
}
	
EWXWEXPORT(int, wxHtmlHelpController_DisplayBlock)(void* _obj, int blockNo)
{
	return (int)((wxHtmlHelpController*)_obj)->DisplayBlock((long)blockNo);
}
	
EWXWEXPORT(void, wxHtmlHelpController_SetFrameParameters)(void* _obj, void* title, int width, int height, int pos_x, int pos_y, int newFrameEachTime)
{
	((wxHtmlHelpController*)_obj)->SetFrameParameters((char*) title, wxSize(width, height), wxPoint(pos_x, pos_y), newFrameEachTime != 0);
}
	
EWXWEXPORT(void*, wxHtmlHelpController_GetFrameParameters)(void* _obj, void* title, int* width, int* height, int* pos_x, int* pos_y, int* newFrameEachTime)
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
	
EWXWEXPORT(int, wxHtmlHelpController_Quit)(void* _obj)
{
	return (int)((wxHtmlHelpController*)_obj)->Quit();
}
	
}
