#include "wrapper.h"
#include "wx/wave.h"

/* testing */
// #define wxUSE_WAVE 0

/*-----------------------------------------------------------------------------
  We want to include the function signatures always -- even on 
  systems that don't support openGL. This means that every function body is
  surrounded by #ifdef wxUSE_WAVE directives :-(
-----------------------------------------------------------------------------*/
#if defined(wxUSE_WAVE) && (wxUSE_WAVE==0)
# undef wxUSE_WAVE
#endif

#ifndef wxUSE_WAVE
# define wxWave      void
#endif



extern "C" {

/*-----------------------------------------------------------------------------
  Wave
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxWave*,wxWave_Create)( wxString* fileName, bool isResource )  
{
#ifdef wxUSE_WAVE 
  return new wxWave(*fileName,isResource);
#else
  return NULL;
#endif
}

EWXWEXPORT(void,wxWave_Delete)(wxWave* self)  
{
#ifdef wxUSE_WAVE 
  if (self) delete self;
#endif
}

EWXWEXPORT(bool,wxWave_IsOk)(wxWave* self)  
{
#ifdef wxUSE_WAVE 
  return self->IsOk();
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxWave_Play)(wxWave* self, bool async, bool looped )  
{
#ifdef wxUSE_WAVE 
  return self->Play(async,looped);
#else
  return false;
#endif
}


}


