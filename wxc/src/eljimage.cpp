#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxImage_CreateDefault)()
{
	return (void*) new wxImage();
}

EWXWEXPORT(void*, wxImage_CreateSized)(int width, int height)
{
	return (void*) new wxImage(width, height);
}

EWXWEXPORT(void*, wxImage_CreateFromByteString) (const char* data, size_t length,int type)
{
	wxMemoryInputStream in(data,length);
	return (void*) new wxImage(in, type);
}

EWXWEXPORT(void*, wxImage_CreateFromLazyByteString) (const char* data, size_t length,int type)
{
	wxMemoryInputStream in(data,length);
	return (void*) new wxImage(in, type);
}

EWXWEXPORT(size_t, wxImage_ConvertToByteString) (wxImage* _obj, int type, char* data )
{
	wxMemoryOutputStream out;
	_obj->SaveFile(out, type);
	size_t len = out.GetLength();
        return out.CopyTo(data, len);
}

EWXWEXPORT(size_t, wxImage_ConvertToLazyByteString) (wxImage* _obj, int type, char* data )
{
	wxMemoryOutputStream out;
	_obj->SaveFile(out, type);
	size_t len = out.GetLength();
        return out.CopyTo(data, len);
}

EWXWEXPORT(void*, wxImage_CreateFromData)(int width, int height, void* data)
{
	return (void*) new wxImage(width, height, (unsigned char*)data, true);
}

EWXWEXPORT(void*, wxImage_CreateFromFile)(void* name)
{
	return (void*) new wxImage((wxChar*)name);
}

EWXWEXPORT(void*, wxImage_CreateFromBitmap)(void* bitmap)
{
	return (void*) new wxImage(((wxBitmap*)bitmap)->ConvertToImage());
}

EWXWEXPORT(void, wxImage_ConvertToBitmap)(void* _obj, void* bitmap)
{
	wxBitmap tmp(*((wxImage*)_obj));
	*((wxBitmap*)bitmap) = tmp;
}
	
EWXWEXPORT(void, wxImage_Initialize)(void* _obj, int width, int height)
{
	((wxImage*)_obj)->Create(width, height);
}
	
EWXWEXPORT(void, wxImage_InitializeFromData)(void* _obj, int width, int height, void* data)
{
	((wxImage*)_obj)->Create(width, height, (unsigned char*)data, true);
}
	
EWXWEXPORT(void, wxImage_Destroy)(void* _obj)
{
	((wxImage*)_obj)->Destroy();
}
	
EWXWEXPORT(void, wxImage_GetSubImage)(void* _obj, int x, int y, int w, int h, void* image)
{
	*((wxImage*)image) = ((wxImage*)_obj)->GetSubImage(wxRect(x, y, w, h));
}
	
EWXWEXPORT(void, wxImage_Paste)(void* _obj, void* image, int x, int y)
{
	((wxImage*)_obj)->Paste(*((wxImage*)image), x, y);
}
	
EWXWEXPORT(void, wxImage_Scale)(void* _obj, int width, int height, void* image)
{
	*((wxImage*)image) = ((wxImage*)_obj)->Scale(width, height);
}
	
EWXWEXPORT(void, wxImage_Rescale)(void* _obj, int width, int height)
{
	((wxImage*)_obj)->Rescale(width, height);
}
	
EWXWEXPORT(void, wxImage_Rotate)(void* _obj, double angle, int c_x, int c_y, int interpolating, void* offset_after_rotation, void* image)
{
	*((wxImage*)image) = ((wxImage*)_obj)->Rotate(angle, wxPoint(c_x, c_y), interpolating != 0, (wxPoint*)offset_after_rotation);
}
	
EWXWEXPORT(void, wxImage_Rotate90)(void* _obj, int clockwise, void* image)
{
	*((wxImage*)image) = ((wxImage*)_obj)->Rotate90(clockwise != 0);
}
	
EWXWEXPORT(void, wxImage_Mirror)(void* _obj, int horizontally, void* image)
{
	*((wxImage*)image) = ((wxImage*)_obj)->Mirror(horizontally != 0);
}
	
EWXWEXPORT(void, wxImage_Replace)(void* _obj, char r1, char g1, char b1, char r2, char g2, char b2)
{
	((wxImage*)_obj)->Replace(r1, g1, b1, r2, g2, b2);
}
	
EWXWEXPORT(void, wxImage_SetRGB)(void* _obj, int x, int y, char r, char g, char b)
{
	((wxImage*)_obj)->SetRGB(x, y, r, g, b);
}
	
EWXWEXPORT(char, wxImage_GetRed)(void* _obj, int x, int y)
{
	return ((wxImage*)_obj)->GetRed(x, y);
}
	
EWXWEXPORT(char, wxImage_GetGreen)(void* _obj, int x, int y)
{
	return ((wxImage*)_obj)->GetGreen(x, y);
}
	
EWXWEXPORT(char, wxImage_GetBlue)(void* _obj, int x, int y)
{
	return ((wxImage*)_obj)->GetBlue(x, y);
}
	
EWXWEXPORT(int, wxImage_CanRead)(void* name)
{
	return (int)wxImage::CanRead((wxChar*)name);
}
	
EWXWEXPORT(int, wxImage_LoadFile)(void* _obj, void* name, int type)
{
	return (int)((wxImage*)_obj)->LoadFile((wxChar*)name, (long)type);
}
	
EWXWEXPORT(int, wxImage_SaveFile)(void* _obj, void* name, int type)
{
	return (int)((wxImage*)_obj)->SaveFile((wxChar*)name, (long)type);
}
	
EWXWEXPORT(int, wxImage_Ok)(void* _obj)
{
	return (int)((wxImage*)_obj)->Ok();
}
	
EWXWEXPORT(int, wxImage_GetWidth)(void* _obj)
{
	return ((wxImage*)_obj)->GetWidth();
}
	
EWXWEXPORT(int, wxImage_GetHeight)(void* _obj)
{
	return ((wxImage*)_obj)->GetHeight();
}
	
EWXWEXPORT(void*, wxImage_GetData)(void* _obj)
{
	return (void*)((wxImage*)_obj)->GetData();
}
	
EWXWEXPORT(void, wxImage_SetData)(void* _obj, void* data)
{
	((wxImage*)_obj)->SetData((unsigned char*)data);
}
	
EWXWEXPORT(void, wxImage_SetDataAndSize)(void* _obj, char *data, int new_width, int new_height)
{
	((wxImage*)_obj)->SetData((unsigned char*)data, new_width, new_height);
}
	
EWXWEXPORT(void, wxImage_SetMaskColour)(void* _obj, char r, char g, char b)
{
	((wxImage*)_obj)->SetMaskColour(r, g, b);
}
	
EWXWEXPORT(char, wxImage_GetMaskRed)(void* _obj)
{
	return ((wxImage*)_obj)->GetMaskRed();
}
	
EWXWEXPORT(char, wxImage_GetMaskGreen)(void* _obj)
{
	return ((wxImage*)_obj)->GetMaskGreen();
}
	
EWXWEXPORT(char, wxImage_GetMaskBlue)(void* _obj)
{
	return ((wxImage*)_obj)->GetMaskBlue();
}
	
EWXWEXPORT(void, wxImage_SetMask)(void* _obj, int mask)
{
	((wxImage*)_obj)->SetMask(mask != 0);
}
	
EWXWEXPORT(int, wxImage_HasMask)(void* _obj)
{
	return (int)((wxImage*)_obj)->HasMask();
}
	
EWXWEXPORT(int, wxImage_CountColours)(void* _obj, int stopafter)
{
	return ((wxImage*)_obj)->CountColours((long)stopafter);
}

EWXWEXPORT (int, wxImage_GetOption)(wxImage* _obj, wxString* key, wxChar* out) {
	wxString result = _obj->GetOption(*key);
	return copyStrToBuf(out, result);
}

EWXWEXPORT (int, wxImage_GetOptionInt)(wxImage* _obj, wxString* key) {
	return  _obj->GetOptionInt(*key);
}

EWXWEXPORT(void, wxImage_SetOption)(wxImage* _obj, wxString* key, wxString* value) {
	_obj->SetOption(*key, *value);
}

EWXWEXPORT(void, wxImage_SetOptionInt)(wxImage* _obj, wxString* key, int value) {
	_obj->SetOption(*key, value);
}

EWXWEXPORT(int, wxImage_HasOption)(wxImage* _obj, wxString* key)
{
	return _obj->HasOption(*key);
}

}
