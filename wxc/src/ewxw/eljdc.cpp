#include "wrapper.h"
#include "wx/metafile.h"

extern "C"
{

EWXWEXPORT(void, wxDC_Delete) (void* _obj)
{
	delete (wxDC*) _obj;
}

EWXWEXPORT(void, wxDC_BeginDrawing)(void* _obj)
{
	((wxDC*)_obj)->BeginDrawing();
}
	
EWXWEXPORT(void, wxDC_EndDrawing)(void* _obj)
{
	((wxDC*)_obj)->EndDrawing();
}
	
EWXWEXPORT(void, wxDC_FloodFill)(void* _obj, int x, int y, void* col, int style)
{
	((wxDC*)_obj)->FloodFill((wxCoord)x, (wxCoord)y, *((wxColour*)col), style);
}
	
EWXWEXPORT(int, wxDC_GetPixel)(void* _obj, int x, int y, void* col)
{
	return ((wxDC*)_obj)->GetPixel((wxCoord)x, (wxCoord)y, (wxColour*)col);
}
	
EWXWEXPORT(void, wxDC_DrawLine)(void* _obj, int x1, int y1, int x2, int y2)
{
	((wxDC*)_obj)->DrawLine((wxCoord)x1, (wxCoord)y1, (wxCoord)x2, (wxCoord)y2);
}
	
EWXWEXPORT(void, wxDC_CrossHair)(void* _obj, int x, int y)
{
	((wxDC*)_obj)->CrossHair((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void, wxDC_DrawArc)(void* _obj, int x1, int y1, int x2, int y2, int xc, int yc)
{
	((wxDC*)_obj)->DrawArc((wxCoord)x1, (wxCoord)y1, (wxCoord)x2, (wxCoord)y2, (wxCoord)xc, (wxCoord)yc);
}
	
EWXWEXPORT(void, wxDC_DrawCheckMark)(void* _obj, int x, int y, int width, int height)
{
	((wxDC*)_obj)->DrawCheckMark((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(void, wxDC_DrawEllipticArc)(void* _obj, int x, int y, int w, int h, double sa, double ea)
{
	((wxDC*)_obj)->DrawEllipticArc((wxCoord)x, (wxCoord)y, (wxCoord)w, (wxCoord)h, sa, ea);
}
	
EWXWEXPORT(void, wxDC_DrawPoint)(void* _obj, int x, int y)
{
	((wxDC*)_obj)->DrawPoint((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void, wxDC_DrawLines)(void* _obj, int n, void* x, void* y, int xoffset, int yoffset)
{
	wxPoint* lst = (wxPoint*)malloc (n * sizeof(wxPoint));
	
	for (int i = 0; i < n; i++)
		lst[i] = wxPoint(((int*)x)[i], ((int*)y)[i]);
	
	((wxDC*)_obj)->DrawLines(n, lst, (wxCoord)xoffset, (wxCoord)yoffset);
	
	free (lst);
}
	
EWXWEXPORT(void, wxDC_DrawPolygon)(void* _obj, int n, void* x, void* y, int xoffset, int yoffset, int fillStyle)
{
	wxPoint* lst = (wxPoint*)malloc (n * sizeof(wxPoint));
	
	for (int i = 0; i < n; i++)
		lst[i] = wxPoint(((int*)x)[i], ((int*)y)[i]);
	
	((wxDC*)_obj)->DrawPolygon(n, lst, (wxCoord)xoffset, (wxCoord)yoffset, fillStyle);
	
	free (lst);
}
	
EWXWEXPORT(void, wxDC_DrawRectangle)(void* _obj, int x, int y, int width, int height)
{
	((wxDC*)_obj)->DrawRectangle((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(void, wxDC_DrawRoundedRectangle)(void* _obj, int x, int y, int width, int height, double radius)
{
	((wxDC*)_obj)->DrawRoundedRectangle((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height, radius);
}
	
EWXWEXPORT(void, wxDC_DrawCircle)(void* _obj, int x, int y, int radius)
{
	((wxDC*)_obj)->DrawCircle((wxCoord)x, (wxCoord)y, (wxCoord)radius);
}
	
EWXWEXPORT(void, wxDC_DrawEllipse)(void* _obj, int x, int y, int width, int height)
{
	((wxDC*)_obj)->DrawEllipse((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(void, wxDC_DrawIcon)(void* _obj, void* icon, int x, int y)
{
	((wxDC*)_obj)->DrawIcon(*((wxIcon*)icon), (wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void, wxDC_DrawBitmap)(void* _obj, void* bmp, int x, int y, int useMask)
{
	((wxDC*)_obj)->DrawBitmap(*((wxBitmap*)bmp), (wxCoord)x, (wxCoord)y, useMask != 0);
}
	
EWXWEXPORT(void, wxDC_DrawText)(void* _obj, void* text, int x, int y)
{
	((wxDC*)_obj)->DrawText((wxChar*)text, (wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void, wxDC_DrawRotatedText)(void* _obj, void* text, int x, int y, double angle)
{
	((wxDC*)_obj)->DrawRotatedText((wxChar*)text, (wxCoord)x, (wxCoord)y, angle);
}
	
EWXWEXPORT(int, wxDC_Blit)(void* _obj, int xdest, int ydest, int width, int height, void* source, int xsrc, int ysrc, int rop, int useMask)
{
	return (int)((wxDC*)_obj)->Blit((wxCoord)xdest, (wxCoord)ydest, (wxCoord)width, (wxCoord)height, (wxDC*)source, (wxCoord)xsrc, (wxCoord)ysrc, rop, useMask != 0);
}
	
EWXWEXPORT(void, wxDC_Clear)(void* _obj)
{
	((wxDC*)_obj)->Clear();
}
	
EWXWEXPORT(int, wxDC_StartDoc)(void* _obj, void* msg)
{
	return (int)((wxDC*)_obj)->StartDoc((wxChar*)msg);
}
	
EWXWEXPORT(void, wxDC_EndDoc)(void* _obj)
{
	((wxDC*)_obj)->EndDoc();
}
	
EWXWEXPORT(void, wxDC_StartPage)(void* _obj)
{
	((wxDC*)_obj)->StartPage();
}
	
EWXWEXPORT(void, wxDC_EndPage)(void* _obj)
{
	((wxDC*)_obj)->EndPage();
}
	
EWXWEXPORT(void, wxDC_SetFont)(void* _obj, void* font)
{
	((wxDC*)_obj)->SetFont(*((wxFont*)font));
}
	
EWXWEXPORT(void, wxDC_SetPen)(void* _obj, void* pen)
{
	((wxDC*)_obj)->SetPen(*((wxPen*)pen));
}
	
EWXWEXPORT(void, wxDC_SetBrush)(void* _obj, void* brush)
{
	((wxDC*)_obj)->SetBrush(*((wxBrush*)brush));
}
	
EWXWEXPORT(void, wxDC_SetBackground)(void* _obj, void* brush)
{
	((wxDC*)_obj)->SetBackground(*((wxBrush*)brush));
}
	
EWXWEXPORT(void, wxDC_SetBackgroundMode)(void* _obj, int mode)
{
	((wxDC*)_obj)->SetBackgroundMode(mode);
}
	
EWXWEXPORT(void, wxDC_SetPalette)(void* _obj, void* palette)
{
	((wxDC*)_obj)->SetPalette(*((wxPalette*)palette));
}
	
EWXWEXPORT(void, wxDC_SetClippingRegion)(void* _obj, int x, int y, int width, int height)
{
	((wxDC*)_obj)->SetClippingRegion((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(void, wxDC_SetClippingRegionFromRegion)(void* _obj, void* region)
{
	((wxDC*)_obj)->SetClippingRegion(*((wxRegion*)region));
}
	
EWXWEXPORT(void, wxDC_DestroyClippingRegion)(void* _obj)
{
	((wxDC*)_obj)->DestroyClippingRegion();
}
	
EWXWEXPORT(void, wxDC_GetClippingBox)(void* _obj, void* x, void* y, void* w, void* h)
{
	((wxDC*)_obj)->GetClippingBox((wxCoord*)x, (wxCoord*)y, (wxCoord*)w, (wxCoord*)h);
}
	
EWXWEXPORT(int, wxDC_GetCharHeight)(void* _obj)
{
	return (int)((wxDC*)_obj)->GetCharHeight();
}
	
EWXWEXPORT(int, wxDC_GetCharWidth)(void* _obj)
{
	return (int)((wxDC*)_obj)->GetCharWidth();
}
	
EWXWEXPORT(void, wxDC_GetTextExtent)(void* _obj, void* string, void* x, void* y, void* descent, void* externalLeading, void* theFont)
{
	((wxDC*)_obj)->GetTextExtent((wxChar*)string, (wxCoord*)x, (wxCoord*)y, (wxCoord*)descent, (wxCoord*)externalLeading, (wxFont*)theFont);
}
	
EWXWEXPORT(void, wxDC_GetSize)(void* _obj, void* width, void* height)
{
	((wxDC*)_obj)->GetSize((int*)width, (int*)height);
}
	
EWXWEXPORT(void, wxDC_GetSizeMM)(void* _obj, void* width, void* height)
{
	((wxDC*)_obj)->GetSizeMM((int*)width, (int*)height);
}
	
EWXWEXPORT(int, wxDC_DeviceToLogicalX)(void* _obj, int x)
{
	return (int)((wxDC*)_obj)->DeviceToLogicalX((wxCoord)x);
}
	
EWXWEXPORT(int, wxDC_DeviceToLogicalY)(void* _obj, int y)
{
	return (int)((wxDC*)_obj)->DeviceToLogicalY((wxCoord)y);
}
	
EWXWEXPORT(int, wxDC_DeviceToLogicalXRel)(void* _obj, int x)
{
	return (int)((wxDC*)_obj)->DeviceToLogicalXRel((wxCoord)x);
}
	
EWXWEXPORT(int, wxDC_DeviceToLogicalYRel)(void* _obj, int y)
{
	return (int)((wxDC*)_obj)->DeviceToLogicalYRel((wxCoord)y);
}
	
EWXWEXPORT(int, wxDC_LogicalToDeviceX)(void* _obj, int x)
{
	return (int)((wxDC*)_obj)->LogicalToDeviceX((wxCoord)x);
}
	
EWXWEXPORT(int, wxDC_LogicalToDeviceY)(void* _obj, int y)
{
	return (int)((wxDC*)_obj)->LogicalToDeviceY((wxCoord)y);
}
	
EWXWEXPORT(int, wxDC_LogicalToDeviceXRel)(void* _obj, int x)
{
	return (int)((wxDC*)_obj)->LogicalToDeviceXRel((wxCoord)x);
}
	
EWXWEXPORT(int, wxDC_LogicalToDeviceYRel)(void* _obj, int y)
{
	return (int)((wxDC*)_obj)->LogicalToDeviceYRel((wxCoord)y);
}
	
EWXWEXPORT(int, wxDC_CanDrawBitmap)(void* _obj)
{
	return (int)((wxDC*)_obj)->CanDrawBitmap();
}
	
EWXWEXPORT(int, wxDC_CanGetTextExtent)(void* _obj)
{
	return (int)((wxDC*)_obj)->CanGetTextExtent();
}
	
EWXWEXPORT(int, wxDC_GetDepth)(void* _obj)
{
	return ((wxDC*)_obj)->GetDepth();
}
	
EWXWEXPORT(void, wxDC_GetPPI)(void* _obj, void* width, void* height)
{
	wxSize result = ((wxDC*)_obj)->GetPPI();
	*((int*)width)  = result.x;
	*((int*)height) = result.y;
}
	
EWXWEXPORT(int, wxDC_Ok)(void* _obj)
{
	return (int)((wxDC*)_obj)->Ok();
}
	
EWXWEXPORT(int, wxDC_GetBackgroundMode)(void* _obj)
{
	return ((wxDC*)_obj)->GetBackgroundMode();
}
	
EWXWEXPORT(void, wxDC_GetBackground)(void* _obj, void* _ref)
{
	*((wxBrush*)_ref) = ((wxDC*)_obj)->GetBackground();
}
	
EWXWEXPORT(void, wxDC_GetBrush)(void* _obj, void* _ref)
{
	*((wxBrush*)_ref) = ((wxDC*)_obj)->GetBrush();
}
	
EWXWEXPORT(void, wxDC_GetFont)(void* _obj, void* _ref)
{
	*((wxFont*)_ref) = ((wxDC*)_obj)->GetFont();
}
	
EWXWEXPORT(void, wxDC_GetPen)(void* _obj, void* _ref)
{
	*((wxPen*)_ref) = ((wxDC*)_obj)->GetPen();
}
	
EWXWEXPORT(void, wxDC_GetTextBackground)(void* _obj, void* _ref)
{
	*((wxColour*)_ref) = ((wxDC*)_obj)->GetTextBackground();
}
	
EWXWEXPORT(void, wxDC_GetTextForeground)(void* _obj, void* _ref)
{
	*((wxColour*)_ref) = ((wxDC*)_obj)->GetTextForeground();
}
	
EWXWEXPORT(void, wxDC_SetTextForeground)(void* _obj, void* colour)
{
	((wxDC*)_obj)->SetTextForeground(*((wxColour*)colour));
}
	
EWXWEXPORT(void, wxDC_SetTextBackground)(void* _obj, void* colour)
{
	((wxDC*)_obj)->SetTextBackground(*((wxColour*)colour));
}
	
EWXWEXPORT(int, wxDC_GetMapMode)(void* _obj)
{
	return ((wxDC*)_obj)->GetMapMode();
}
	
EWXWEXPORT(void, wxDC_SetMapMode)(void* _obj, int mode)
{
	((wxDC*)_obj)->SetMapMode(mode);
}
	
EWXWEXPORT(void, wxDC_GetUserScale)(void* _obj, void* x, void* y)
{
	((wxDC*)_obj)->GetUserScale((double*)x, (double*)y);
}
	
EWXWEXPORT(void, wxDC_SetUserScale)(void* _obj, double x, double y)
{
	((wxDC*)_obj)->SetUserScale(x, y);
}
	
EWXWEXPORT(void, wxDC_GetLogicalScale)(void* _obj, void* x, void* y)
{
	((wxDC*)_obj)->GetLogicalScale((double*)x, (double*)y);
}
	
EWXWEXPORT(void, wxDC_SetLogicalScale)(void* _obj, double x, double y)
{
	((wxDC*)_obj)->SetLogicalScale(x, y);
}
	
EWXWEXPORT(void, wxDC_GetLogicalOrigin)(void* _obj, void* x, void* y)
{
	((wxDC*)_obj)->GetLogicalOrigin((wxCoord*)x, (wxCoord*)y);
}
	
EWXWEXPORT(void, wxDC_SetLogicalOrigin)(void* _obj, int x, int y)
{
	((wxDC*)_obj)->SetLogicalOrigin((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void, wxDC_GetDeviceOrigin)(void* _obj, void* x, void* y)
{
	((wxDC*)_obj)->GetDeviceOrigin((wxCoord*)x, (wxCoord*)y);
}
	
EWXWEXPORT(void, wxDC_SetDeviceOrigin)(void* _obj, int x, int y)
{
	((wxDC*)_obj)->SetDeviceOrigin((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void, wxDC_SetAxisOrientation)(void* _obj, int xLeftRight, int yBottomUp)
{
	((wxDC*)_obj)->SetAxisOrientation(xLeftRight != 0, yBottomUp != 0);
}
	
EWXWEXPORT(int, wxDC_GetLogicalFunction)(void* _obj)
{
	return ((wxDC*)_obj)->GetLogicalFunction();
}
	
EWXWEXPORT(void, wxDC_SetLogicalFunction)(void* _obj, int function)
{
	((wxDC*)_obj)->SetLogicalFunction(function);
}
	
EWXWEXPORT(void, wxDC_SetOptimization)(void* _obj, int opt)
{
	((wxDC*)_obj)->SetOptimization(opt != 0);
}
	
EWXWEXPORT(int, wxDC_GetOptimization)(void* _obj)
{
	return (int)((wxDC*)_obj)->GetOptimization();
}
	
EWXWEXPORT(void, wxDC_CalcBoundingBox)(void* _obj, int x, int y)
{
	((wxDC*)_obj)->CalcBoundingBox((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void, wxDC_ResetBoundingBox)(void* _obj)
{
	((wxDC*)_obj)->ResetBoundingBox();
}
	
EWXWEXPORT(int, wxDC_MinX)(void* _obj)
{
	return (int)((wxDC*)_obj)->MinX();
}
	
EWXWEXPORT(int, wxDC_MaxX)(void* _obj)
{
	return (int)((wxDC*)_obj)->MaxX();
}
	
EWXWEXPORT(int, wxDC_MinY)(void* _obj)
{
	return (int)((wxDC*)_obj)->MinY();
}
	
EWXWEXPORT(int, wxDC_MaxY)(void* _obj)
{
	return (int)((wxDC*)_obj)->MaxY();
}

EWXWEXPORT(void*, wxWindowDC_Create) (void* win)
{
	return (void*) new wxWindowDC((wxWindow*)win);
}

EWXWEXPORT(void, wxWindowDC_Delete) (void* _obj)
{
	delete (wxWindowDC*)_obj;
}

EWXWEXPORT(void*, wxClientDC_Create) (void* win)
{
	return (void*) new wxClientDC((wxWindow*)win);
}

EWXWEXPORT(void, wxClientDC_Delete) (void* _obj)
{
	delete (wxClientDC*)_obj;
}

EWXWEXPORT(void*, wxPaintDC_Create) (void* win)
{
	return (void*) new wxPaintDC((wxWindow*)win);
}

EWXWEXPORT(void, wxPaintDC_Delete) (void* _obj)
{
	delete (wxPaintDC*)_obj;
}

EWXWEXPORT(void*, wxMemoryDC_Create) ()
{
	return (void*) new wxMemoryDC();
}

EWXWEXPORT(void*, wxMemoryDC_CreateCompatible) (void* dc)
{
	return (void*) new wxMemoryDC((wxDC*) dc);
}

EWXWEXPORT(void, wxMemoryDC_Delete) (void* _obj)
{
	delete (wxMemoryDC*)_obj;
}

EWXWEXPORT(void, wxMemoryDC_SelectObject)(void* _obj, void* bitmap)
{
	((wxMemoryDC*)_obj)->SelectObject(*((wxBitmap*)bitmap));
}
	
EWXWEXPORT(void*, wxScreenDC_Create) ()
{
	return (void*) new wxScreenDC();
}

EWXWEXPORT(void, wxScreenDC_Delete) (void* _obj)
{
	delete (wxScreenDC*)_obj;
}

EWXWEXPORT(int, wxScreenDC_StartDrawingOnTopOfWin)(void* _obj, void* win)
{
	return (int)((wxScreenDC*)_obj)->StartDrawingOnTop((wxWindow*)win);
}
	
EWXWEXPORT(int, wxScreenDC_StartDrawingOnTop)(void* _obj, int l, int t, int w, int h)
{
	wxRect rect(l, t, w, h);
	return (int)((wxScreenDC*)_obj)->StartDrawingOnTop(&rect);
}
	
EWXWEXPORT(int, wxScreenDC_EndDrawingOnTop)(void* _obj)
{
	return (int)((wxScreenDC*)_obj)->EndDrawingOnTop();
}
	
EWXWEXPORT(void*,wxMetafileDC_Create)(void* _file)
{
#if defined(__WXGTK__) || defined(__WXMAC__) 
	return NULL;
#else
	wxString file;
	
	if (_file) file = (wxChar*)_file;

    return (void*)new wxMetafileDC(file);
#endif
}

EWXWEXPORT(void*, wxMetafileDC_Close) (void* _obj)
{
#if defined(__WXGTK__)
	return NULL;
#else
	return (void*)((wxMetafileDC*)_obj)->Close();
#endif
}

EWXWEXPORT(void, wxMetafileDC_Delete) (void* _obj)
{
#if !defined(__WXGTK__)
	delete (wxMetafileDC*)_obj;
#endif
}

EWXWEXPORT(void*,wxMetafile_Create)(void* _file)
{
#if defined(__WXGTK__)
	return NULL;
#else
	wxString file;
	
	if (_file) file = (wxChar*)_file;

    return (void*)new wxMetafile(file);
#endif
}

EWXWEXPORT(int,wxMetafile_SetClipboard)(void* _obj, int width, int height)
{
#if defined(__WXGTK__)
	return 0;
#else
	return (int)((wxMetafile*)_obj)->SetClipboard(width, height);
#endif
}
	
EWXWEXPORT(int,wxMetafile_Play)(void* _obj, void* _dc)
{
#if defined(__WXGTK__)
	return 0;
#else
	return (int)((wxMetafile*)_obj)->Play((wxDC*)_dc);
#endif
}
	
EWXWEXPORT(int,wxMetafile_Ok)(void* _obj)
{
#if defined(__WXGTK__)
	return 0;
#else
	return (int)((wxMetafile*)_obj)->Ok();
#endif
}
	
EWXWEXPORT(void, wxMetafile_Delete) (void* _obj)
{
#if !defined(__WXGTK__)
	delete (wxMetafile*)_obj;
#endif
}

}
