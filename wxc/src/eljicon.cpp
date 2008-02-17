#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxIcon_CreateDefault)()
{
	return (void*) new wxIcon();
}

EWXWEXPORT(void, wxIcon_Delete)(void* _obj)
{
	delete (wxIcon*)_obj;
}

EWXWEXPORT(void*, wxIcon_FromRaw)(void* data, int width, int height)
{
#ifdef __WIN32__
	return (void*) new wxIcon((const wxChar*) data, wxBITMAP_TYPE_ICO, width, height);
#else
	return (void*) new wxIcon((const wxChar*) data, wxBITMAP_TYPE_ANY, width, height);
#endif
}

EWXWEXPORT(void*, wxIcon_FromXPM)(void* data)
{
	return (void*) new wxIcon((const wxChar*) data);
}

EWXWEXPORT(void*, wxIcon_CreateLoad) (void* name, long type, int width, int height)
{
	return (void*) new wxIcon((wxChar*)name, (wxBitmapType)type, width, height);
}

EWXWEXPORT(int, wxIcon_Load)(void* _obj, void* name, long type, int width, int height)
{
#ifdef __WIN32__
	return (int)((wxIcon*)_obj)->LoadFile((wxChar*)name, (wxBitmapType)type, width, height);
#else
	return (int)((wxIcon*)_obj)->LoadFile((wxChar*)name, (wxBitmapType)type);
#endif
}

EWXWEXPORT(void, wxIcon_CopyFromBitmap)(void* _obj, void* bmp)
{
#ifdef __WIN32__
	((wxIcon*)_obj)->CopyFromBitmap(*((wxBitmap*)bmp));
#endif
}

EWXWEXPORT(int, wxIcon_Ok)(void* _obj)
{
	return (int)((wxIcon*)_obj)->Ok();
}

EWXWEXPORT(int, wxIcon_GetDepth)(void* _obj)
{
	return (int)((wxIcon*)_obj)->GetDepth();
}

EWXWEXPORT(int, wxIcon_GetWidth)(void* _obj)
{
	return (int)((wxIcon*)_obj)->GetWidth();
}

EWXWEXPORT(int, wxIcon_GetHeight)(void* _obj)
{
	return (int)((wxIcon*)_obj)->GetHeight();
}

EWXWEXPORT(void, wxIcon_Assign)(void* _obj, void* other)
{
	*((wxIcon*)_obj) = *((wxIcon*)other);
}

EWXWEXPORT(int, wxIcon_IsEqual)(void* _obj, void* other)
{
#if (wxVERSION_NUMBER <= 2800)
	return (int)(*((wxIcon*)_obj) == *((wxIcon*)other));
#else
	wxIcon* icon1 = (wxIcon *)_obj;
	wxIcon* icon2 = (wxIcon *)other;
	wxBitmap bmp1;
	wxBitmap bmp2;
	bmp1.CopyFromIcon(*icon1);
	bmp2.CopyFromIcon(*icon2);
	wxImage image1 = (wxImage)bmp1.ConvertToImage();
	wxImage image2 = (wxImage)bmp2.ConvertToImage();
	wxImage* img1 = &image1;
	wxImage* img2 = &image2;
	if( (icon1->GetWidth() == icon2->GetWidth()) &&
		(icon1->GetHeight() == icon2->GetHeight()) &&
		(icon1->GetDepth() == icon2->GetDepth())){
		bool equal = true;
		for(int sx=0;sx<(icon1->GetWidth());sx++){
			for(int sy=0;sy<(icon1->GetHeight());sy++){
				equal = equal &&
					(img1->GetRed(sx,sy)==img2->GetRed(sx,sy) &&
					img1->GetGreen(sx,sy)==img2->GetGreen(sx,sy) &&
					img1->GetBlue(sx,sy)==img2->GetBlue(sx,sy) &&
					img1->GetAlpha(sx,sy)==img2->GetAlpha(sx,sy));
			}
		}
		return (int)equal;
	} else {
		return 0;
	}
#endif
}

}
