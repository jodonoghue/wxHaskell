/*-----------------------------------------------------------------------------
  GLCanvas
-----------------------------------------------------------------------------*/
TClassDefExtend(wxGLCanvas,wxWindow);

TClass(wxGLCanvas)   wxGLCanvas_Create(TClass(wxWindow) parent, int windowID, TRect(x,y,w,h), int style, TClass(wxString) title, int* attributes, TClass(wxPalette) palette );
TClass(wxGLCanvas)   wxGLCanvas_CreateShared(TClass(wxWindow) parent, TClass(wxGLCanvas) sharedCanvas, int windowID, TRect(x,y,w,h), int style, TClass(wxString) title, int* attributes, TClass(wxPalette) palette );
void   wxGLCanvas_SetCurrent(TSelf(wxGLCanvas) self);
void   wxGLCanvas_SetColour(TSelf(wxGLCanvas) self, TClass(wxColour) colour);
void   wxGLCanvas_SwapBuffers(TSelf(wxGLCanvas) self);  
