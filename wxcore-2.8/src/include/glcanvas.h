/*-----------------------------------------------------------------------------
  GLCanvas
-----------------------------------------------------------------------------*/
TClassDefExtend(wxGLCanvas,wxWindow);
TClassDefExtend(wxGLContext,wxObject);

TClass(wxGLCanvas)   wxGLCanvas_Create( TClass(wxWindow) parent, int windowID, int* attributes, TRect(x,y,w,h), int style, TClass(wxString) title, TClass(wxPalette) palette );
void   wxGLCanvas_SetColour( TSelf(wxGLCanvas) self, TClass(wxColour) colour );
void   wxGLCanvas_SetCurrent( TSelf(wxGLCanvas) self, TClass(wxGLContext) ctxt );
void   wxGLCanvas_SwapBuffers( TSelf(wxGLCanvas) self );  

/*-----------------------------------------------------------------------------
  GLContext
-----------------------------------------------------------------------------*/

TClass(wxGLContext)   wxGLContext_Create( TClass(wxGLCanvas) win, TClass(wxGLContext) other );
TClass(wxGLContext)   wxGLContext_CreateFromNull( TClass(wxGLCanvas) win );
void   wxGLContext_SetCurrent( TSelf(wxGLContext) self, TClass(wxGLCanvas) win);
