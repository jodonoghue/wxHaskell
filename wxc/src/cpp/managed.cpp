#include "wrapper.h"

/* safety */
class wxManagedPtr;
#include "wxc_types.h"
#include "managed.h"


/*-----------------------------------------------------------------------------
   Managed pointers are pointed to by ForeignPtr's in Haskell.
   They are basically indirections with a finalization
   function.
-----------------------------------------------------------------------------*/
typedef void (_cdecl *Finalizer)(void* ptr);

class wxManagedPtr 
{
private:
  void*     ptr;
  Finalizer finalizer;

public:
  wxManagedPtr( void* p, Finalizer f ) 
  {
    ptr       = p;
    finalizer = f;
  };

  void* GetPtr() 
  {
    return ptr;
  }

  void NoFinalize()
  {
    finalizer = NULL;
  }

  void Finalize()
  {
    if (ptr!=NULL && finalizer!=NULL) {
      finalizer(ptr);
      finalizer = NULL;
    }
  }

  ~wxManagedPtr() 
  {
    Finalize();
  };
};

extern "C" {
/*-----------------------------------------------------------------------------
  Operations
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,wxManagedPtr_GetPtr)(wxManagedPtr* self)
{
  if (self!=NULL) return self->GetPtr();
             else return NULL;
}

EWXWEXPORT(void,wxManagedPtr_NoFinalize)(wxManagedPtr* self)
{
  if (self!=NULL) {
    self->NoFinalize();
  }
}

EWXWEXPORT(void,wxManagedPtr_Finalize)(wxManagedPtr* self)
{
  if (self!=NULL) {
    self->Finalize();
  }
}

EWXWEXPORT(void,wxManagedPtr_Delete)(wxManagedPtr* self)
{
  if (self!=NULL) {
    delete self;
  }
}

static void _cdecl deleteManagedPtr(wxManagedPtr* mp)
{
  if (mp!=NULL) {
    delete mp;
  }
}

EWXWEXPORT(void*,wxManagedPtr_GetDeleteFunction)()
{
  return (void*)&deleteManagedPtr;  
}



/*-----------------------------------------------------------------------------
  Finalize wxObject
-----------------------------------------------------------------------------*/
static void _cdecl deleteObject( wxObject* obj )
{
  if (obj!=NULL) {
    delete obj;
  }
}

EWXWEXPORT(void,wxObject_SafeDelete)(wxObject* obj)
{
  deleteObject(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromObject)(wxObject* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteObject );
}

/*-----------------------------------------------------------------------------
  Finalize DateTime
-----------------------------------------------------------------------------*/
static void _cdecl deleteDateTime( wxDateTime* obj )
{
  if (obj!=NULL) {
    delete obj;
  }
}


EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromDateTime)(wxDateTime* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteDateTime );
}

/*-----------------------------------------------------------------------------
  Finalize wxGridCellCoordsArray
-----------------------------------------------------------------------------*/
static void _cdecl deleteGridCellCoordsArray( wxGridCellCoordsArray* obj )
{
  if (obj!=NULL) {
    delete obj;
  }
}


EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromGridCellCoordsArray)(wxGridCellCoordsArray* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteGridCellCoordsArray );
}


/*-----------------------------------------------------------------------------
  Finalize wxBitmap
-----------------------------------------------------------------------------*/
EWXWEXPORT(bool,wxBitmap_IsStatic)(wxBitmap* obj)
{
  static int calls=0; calls++;    /* prevent bug in VisualC 6.0 ? */
  return (obj==&wxNullBitmap || obj==NULL);
}

static void _cdecl deleteBitmap( wxBitmap* obj )
{
  if (!wxBitmap_IsStatic(obj)) {
    delete obj;
  }
}

EWXWEXPORT(void,wxBitmap_SafeDelete)(wxBitmap* obj)
{
  deleteBitmap(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromBitmap)(wxBitmap* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteBitmap );
}


/*-----------------------------------------------------------------------------
  Finalize wxIcon
-----------------------------------------------------------------------------*/
EWXWEXPORT(bool,wxIcon_IsStatic)(wxIcon* obj)
{
  static int calls=0; calls++;    /* prevent bug in VisualC 6.0 ? */
  return (obj==NULL || obj==&wxNullIcon);
}

static void _cdecl deleteIcon( wxIcon* obj )
{
  if (!wxIcon_IsStatic(obj)) {
    delete obj;
  }
}

EWXWEXPORT(void,wxIcon_SafeDelete)(wxIcon* obj)
{
  deleteIcon(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromIcon)(wxIcon* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteIcon );
}


/*-----------------------------------------------------------------------------
  Finalize wxBrush
-----------------------------------------------------------------------------*/
/* defined as macro to type without casts ... sigh */
/* use indirection array to let wx properly initialize the object pointers */

#define IsStatic(obj,statics) \
  { \
    int i; \
    if (obj==NULL) return true; \
    for( i = 0; statics[i] != NULL; i++ ) \
    { \
      if (*statics[i] == obj) return true; \
    }  \
    return false; \
  }


#if (wxVERSION_NUMBER < 2800)
static wxBrush* wxNULL_BRUSH = &wxNullBrush;

static wxBrush** staticsBrush[] =
    {&wxNULL_BRUSH
    ,&wxBLUE_BRUSH
    ,&wxGREEN_BRUSH
    ,&wxWHITE_BRUSH
    ,&wxBLACK_BRUSH
    ,&wxGREY_BRUSH
    ,&wxMEDIUM_GREY_BRUSH
    ,&wxLIGHT_GREY_BRUSH
    ,&wxTRANSPARENT_BRUSH
    ,&wxCYAN_BRUSH
    ,&wxRED_BRUSH
    ,NULL
    };
#else
/* VS2005 doesn't allow taking the address of the returned value of 
   a function call. The code below ensures that there's actually an
   address assigned in each case.

   Just to compilcate matters, all of the values are now const
   pointers. This is correct, but implies lots of downstream changes
   so (horrible hack) I get rid of the 'constness'...
 */
static wxBrush* wxNULL_BRUSH         = const_cast<wxBrush*>(&wxNullBrush);
static wxBrush* wxcBLUE_BRUSH        = const_cast<wxBrush*>(wxBLUE_BRUSH);
static wxBrush* wxcGREEN_BRUSH       = const_cast<wxBrush*>(wxGREEN_BRUSH);
static wxBrush* wxcWHITE_BRUSH       = const_cast<wxBrush*>(wxWHITE_BRUSH);
static wxBrush* wxcBLACK_BRUSH       = const_cast<wxBrush*>(wxBLACK_BRUSH);
static wxBrush* wxcGREY_BRUSH        = const_cast<wxBrush*>(wxGREY_BRUSH);
static wxBrush* wxcMEDIUM_GREY_BRUSH = const_cast<wxBrush*>(wxMEDIUM_GREY_BRUSH);
static wxBrush* wxcLIGHT_GREY_BRUSH  = const_cast<wxBrush*>(wxLIGHT_GREY_BRUSH);
static wxBrush* wxcTRANSPARENT_BRUSH = const_cast<wxBrush*>(wxTRANSPARENT_BRUSH);
static wxBrush* wxcCYAN_BRUSH        = const_cast<wxBrush*>(wxCYAN_BRUSH);
static wxBrush* wxcRED_BRUSH         = const_cast<wxBrush*>(wxRED_BRUSH);

static wxBrush** staticsBrush[] =
    {&wxNULL_BRUSH
    ,&wxcBLUE_BRUSH
    ,&wxcGREEN_BRUSH
    ,&wxcWHITE_BRUSH
    ,&wxcBLACK_BRUSH
    ,&wxcGREY_BRUSH
    ,&wxcMEDIUM_GREY_BRUSH
    ,&wxcLIGHT_GREY_BRUSH
    ,&wxcTRANSPARENT_BRUSH
    ,&wxcCYAN_BRUSH
    ,&wxcRED_BRUSH
    ,NULL
    };

#endif

EWXWEXPORT(bool,wxBrush_IsStatic)(wxBrush* obj)
{
  IsStatic(obj,staticsBrush);  
}

static void _cdecl deleteBrush( wxBrush* obj )
{
  if (!wxBrush_IsStatic(obj)) {
    delete obj;
  }
}

EWXWEXPORT(void,wxBrush_SafeDelete)(wxBrush* obj)
{
  deleteBrush(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromBrush)(wxBrush* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteBrush );
}

/*-----------------------------------------------------------------------------
  Finalize wxColour
-----------------------------------------------------------------------------*/
static wxColour* wxNULL_COLOUR = &wxNullColour;

#if (wxVERSION_NUMBER < 2800)
static wxColour** staticsColour[] = 
    {&wxNULL_COLOUR
    ,&wxBLACK
    ,&wxWHITE
    ,&wxRED
    ,&wxBLUE
    ,&wxGREEN
    ,&wxCYAN
    ,&wxLIGHT_GREY
    ,NULL
    };
#else
static wxColour* wxcBLACK      = const_cast<wxColour *>(wxBLACK);
static wxColour* wxcWHITE      = const_cast<wxColour *>(wxWHITE);
static wxColour* wxcRED        = const_cast<wxColour *>(wxRED);
static wxColour* wxcBLUE       = const_cast<wxColour *>(wxBLUE);
static wxColour* wxcGREEN      = const_cast<wxColour *>(wxGREEN);
static wxColour* wxcCYAN       = const_cast<wxColour *>(wxCYAN);
static wxColour* wxcLIGHT_GREY = const_cast<wxColour *>(wxLIGHT_GREY);

static wxColour** staticsColour[] = 
    {&wxNULL_COLOUR
    ,&wxcBLACK
    ,&wxcWHITE
    ,&wxcRED
    ,&wxcBLUE
    ,&wxcGREEN
    ,&wxcCYAN
    ,&wxcLIGHT_GREY
    ,NULL
    };
#endif

EWXWEXPORT(bool,wxColour_IsStatic)(wxColour* obj)
{
  IsStatic(obj,staticsColour);  
}

static void _cdecl deleteColour( wxColour* obj )
{
  if (!wxColour_IsStatic(obj)) {
    delete obj;
  }
}

EWXWEXPORT(void,wxColour_SafeDelete)(wxColour* obj)
{
  deleteColour(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromColour)(wxColour* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteColour );
}


/*-----------------------------------------------------------------------------
  Finalize wxCursor
-----------------------------------------------------------------------------*/
wxCursor*** staticsCursor(void)
{
#if (wxVERSION_NUMBER < 2800)
static wxCursor* wxNULL_CURSOR = &wxNullCursor;

static wxCursor** staticsCursor[] = 
    {&wxNULL_CURSOR
    ,&wxSTANDARD_CURSOR
    ,&wxHOURGLASS_CURSOR
    ,&wxCROSS_CURSOR
    ,NULL
    };
#else
static wxCursor* wxNULL_CURSOR       = const_cast<wxCursor *>(&wxNullCursor);
static wxCursor* wxcSTANDARD_CURSOR  = const_cast<wxCursor *>(wxSTANDARD_CURSOR);
static wxCursor* wxcHOURGLASS_CURSOR = const_cast<wxCursor *>(wxHOURGLASS_CURSOR);
//static wxCursor* wxcCROSS_CURSOR     = const_cast<wxCursor *>(wxCROSS_CURSOR);

static wxCursor** staticsCursor[] = 
    {&wxNULL_CURSOR
    ,&wxcSTANDARD_CURSOR
    ,&wxcHOURGLASS_CURSOR
     //,&wxcCROSS_CURSOR
    ,NULL
    };
#endif
  return staticsCursor;
}

EWXWEXPORT(bool,wxCursor_IsStatic)(wxCursor* obj)
{
  IsStatic(obj,staticsCursor());  
}

static void _cdecl deleteCursor( wxCursor* obj )
{
  if (!wxCursor_IsStatic(obj)) {
    delete obj;
  }
}

EWXWEXPORT(void,wxCursor_SafeDelete)(wxCursor* obj)
{
  deleteCursor(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromCursor)(wxCursor* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteCursor );
}


/*-----------------------------------------------------------------------------
  Finalize wxFont
-----------------------------------------------------------------------------*/

#if (wxVERSION_NUMBER < 2800)
static wxFont* wxNULL_FONT  = &wxNullFont;

static wxFont** staticsFont[] = 
    {&wxNULL_FONT
    ,&wxNORMAL_FONT
    ,&wxSMALL_FONT
    ,&wxITALIC_FONT
    ,&wxSWISS_FONT
    ,NULL
    };
#else
static wxFont* wxNULL_FONT    = const_cast<wxFont *>(&wxNullFont);
// static wxFont* wxcNORMAL_FONT = const_cast<wxFont *>(wxNORMAL_FONT);
//static wxFont* wxcSMALL_FONT  = const_cast<wxFont *>(wxSMALL_FONT);
//static wxFont* wxcITALIC_FONT = const_cast<wxFont *>(wxITALIC_FONT);
//static wxFont* wxcSWISS_FONT  = const_cast<wxFont *>(wxSWISS_FONT);

static wxFont** staticsFont[] = 
    {&wxNULL_FONT
     //    ,&wxcNORMAL_FONT
     //,&wxcSMALL_FONT
     //,&wxcITALIC_FONT
     //,&wxcSWISS_FONT
    ,NULL
    };
#endif

EWXWEXPORT(bool,wxFont_IsStatic)(wxFont* obj)
{
  IsStatic(obj,staticsFont);  
}

static void _cdecl deleteFont( wxFont* obj )
{
  if (!wxFont_IsStatic(obj)) {
    delete obj;
  }
}

EWXWEXPORT(void,wxFont_SafeDelete)(wxFont* obj)
{
  deleteFont(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromFont)(wxFont* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteFont );
}


/*-----------------------------------------------------------------------------
  Finalize wxPen
-----------------------------------------------------------------------------*/
static wxPen* wxNULL_PEN = &wxNullPen;

#if (wxVERSION_NUMBER < 2800)
static wxPen** staticsPen[] = 
    {&wxNULL_PEN
    ,&wxRED_PEN
    ,&wxCYAN_PEN
    ,&wxGREEN_PEN
    ,&wxBLACK_PEN
    ,&wxWHITE_PEN
    ,&wxTRANSPARENT_PEN
    ,&wxBLACK_DASHED_PEN
    ,&wxGREY_PEN
    ,&wxMEDIUM_GREY_PEN
    ,&wxLIGHT_GREY_PEN
    ,NULL
    };
#else
static wxPen* wxcRED_PEN          = const_cast<wxPen *>(wxRED_PEN);
static wxPen* wxcCYAN_PEN         = const_cast<wxPen *>(wxCYAN_PEN);
static wxPen* wxcGREEN_PEN        = const_cast<wxPen *>(wxGREEN_PEN);
static wxPen* wxcBLACK_PEN        = const_cast<wxPen *>(wxBLACK_PEN);
static wxPen* wxcWHITE_PEN        = const_cast<wxPen *>(wxWHITE_PEN);
static wxPen* wxcTRANSPARENT_PEN  = const_cast<wxPen *>(wxTRANSPARENT_PEN);
static wxPen* wxcBLACK_DASHED_PEN = const_cast<wxPen *>(wxBLACK_DASHED_PEN);
static wxPen* wxcGREY_PEN         = const_cast<wxPen *>(wxGREY_PEN);
static wxPen* wxcMEDIUM_GREY_PEN  = const_cast<wxPen *>(wxMEDIUM_GREY_PEN);
static wxPen* wxcLIGHT_GREY_PEN   = const_cast<wxPen *>(wxLIGHT_GREY_PEN);

static wxPen** staticsPen[] = 
    {&wxNULL_PEN
    ,&wxcRED_PEN
    ,&wxcCYAN_PEN
    ,&wxcGREEN_PEN
    ,&wxcBLACK_PEN
    ,&wxcWHITE_PEN
    ,&wxcTRANSPARENT_PEN
    ,&wxcBLACK_DASHED_PEN
    ,&wxcGREY_PEN
    ,&wxcMEDIUM_GREY_PEN
    ,&wxcLIGHT_GREY_PEN
    ,NULL
    };
#endif

EWXWEXPORT(bool,wxPen_IsStatic)(wxPen* obj)
{
  IsStatic(obj,staticsPen);  
}

static void _cdecl deletePen( wxPen* obj )
{
  if (!wxPen_IsStatic(obj)) {
    delete obj;
  }
}

EWXWEXPORT(void,wxPen_SafeDelete)(wxPen* obj)
{
  deletePen(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromPen)(wxPen* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deletePen );
}

/* extern "C" */
}
