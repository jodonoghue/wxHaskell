#include "wrapper.h"
#include "wx/glcanvas.h"

/* testing */
// #define wxUSE_GLCANVAS 0

/*-----------------------------------------------------------------------------
  We want to include the function signatures always -- even on 
  systems that don't support openGL. This means that every function body is
  surrounded by #ifdef wxUSE_GLCANVAS directives :-(
-----------------------------------------------------------------------------*/
#if defined(wxUSE_GLCANVAS) && (wxUSE_GLCANVAS==0)
# undef wxUSE_GLCANVAS
#endif

#ifndef wxUSE_GLCANVAS
# define wxGLCanvas      void
#endif



extern "C" {

/*-----------------------------------------------------------------------------
  GLCanvas
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxGLCanvas*,wxGLCanvas_Create)(wxWindow* parent, int windowID,
                                        int x, int y, int w, int h, int style,
                                        wxString* title, int* attributes,
                                        wxPalette* palette )  
{
#ifdef wxUSE_GLCANVAS 
  return new wxGLCanvas(parent,windowID,wxPoint(x,y),wxSize(w,h),style,
    (title ? *title : wxString("GLCanvas")), attributes, 
    (palette ? *palette : wxNullPalette));
#else
  return NULL;
#endif
}

EWXWEXPORT(wxGLCanvas*,wxGLCanvas_CreateShared)(wxWindow* parent, wxGLCanvas* sharedCanvas,
                                        int windowID,int x, int y, int w, int h, int style,
                                        wxString* title, int* attributes,
                                        wxPalette* palette )  
{
#ifdef wxUSE_GLCANVAS 
  return new wxGLCanvas(parent,sharedCanvas,windowID,wxPoint(x,y),wxSize(w,h),style,
    (title ? *title : wxString("GLCanvas")), attributes, 
    (palette ? *palette : wxNullPalette));
#else
  return NULL;
#endif
}


EWXWEXPORT(void,wxGLCanvas_SetCurrent)(wxGLCanvas* self)  
{
#ifdef wxUSE_GLCANVAS 
  self->SetCurrent();
#endif
}

EWXWEXPORT(void,wxGLCanvas_SetColour)(wxGLCanvas* self, wxColour* colour)  
{
#ifdef wxUSE_GLCANVAS 
  wxString name = wxTheColourDatabase->FindName( *colour );
  if (name.IsEmpty()) 
    self->SetColour(wxString("WHITE"));
  else
    self->SetColour(name);
#endif
}

EWXWEXPORT(void,wxGLCanvas_SwapBuffers)(wxGLCanvas* self)  
{
#ifdef wxUSE_GLCANVAS 
  self->SwapBuffers();
#endif
}

}


