#include "wrapper.h"
#if (wxVERSION_NUMBER >= 2500)
#include "wx/sound.h"
#else /* (wxVERSION_NUMBER >= 2500) */
#include "wx/wave.h"
#endif /* (wxVERSION_NUMBER >= 2500) */

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

#if defined(wxUSE_SOUND) && (wxUSE_SOUND==0)
# undef wxUSE_SOUND
#endif

#ifndef wxUSE_WAVE
# define wxWave      void
#endif

#ifndef wxUSE_SOUND
# define wxSound      void
#endif

extern "C" {

#if (wxVERSION_NUMBER >= 2500)
/*-----------------------------------------------------------------------------
  Sound
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxSound*,wxSound_Create)( wxString* fileName, bool isResource )  
{
#ifdef wxUSE_SOUND 
  return new wxSound(*fileName,isResource);
#else
  return NULL;
#endif
}

EWXWEXPORT(void,wxSound_Delete)(wxSound* self)  
{
#ifdef wxUSE_SOUND 
  if (self) delete self;
#endif
}

EWXWEXPORT(bool,wxSound_IsOk)(wxSound* self)  
{
#ifdef wxUSE_SOUND 
  return self->IsOk();
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxSound_Play)(wxSound* self, unsigned flag )  
{
#ifdef wxUSE_SOUND 
  return ((wxSoundBase *) self)->Play(flag);
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxSound_PlayCompatible)(wxSound* self, bool async, bool looped )  
{
#if (WXWIN_COMPATIBILITY_2_4==1) && defined(wxUSE_SOUND)
  return ((wxSoundBase *) self)->Play(async,looped);
#else
  return false;
#endif
}

#else /* (wxVERSION_NUMBER >= 2500) */

/*-----------------------------------------------------------------------------
  Wave
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxWave*,wxSound_Create)( wxString* fileName, bool isResource )  
{
#ifdef wxUSE_WAVE 
  return new wxWave(*fileName,isResource);
#else
  return NULL;
#endif
}

EWXWEXPORT(void,wxSound_Delete)(wxWave* self)  
{
#ifdef wxUSE_WAVE 
  if (self) delete self;
#endif
}

EWXWEXPORT(bool,wxSound_IsOk)(wxWave* self)  
{
#ifdef wxUSE_WAVE 
  return self->IsOk();
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxSound_PlayCompatible)(wxWave* self, bool async, bool looped )  
{
#ifdef wxUSE_WAVE 
  return self->Play(async,looped);
#else
  return false;
#endif
}

#endif /* (wxVERSION_NUMBER >= 2500) */

}


