/*-----------------------------------------------------------------------------
  Wave
-----------------------------------------------------------------------------*/
TClassDefExtend(wxWave,wxObject);

/** Usage: @waveCreate fileName isResource@. As yet (Nov 2003) unsupported on MacOS X */
TClass(wxWave)  wxWave_Create( TClass(wxString) fileName, TBool isResource );
void  wxWave_Delete(TSelf(wxWave) self);
TBool  wxWave_IsOk(TSelf(wxWave) self);
TBool  wxWave_Play(TSelf(wxWave) self, TBool async, TBool looped );
