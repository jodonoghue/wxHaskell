#ifndef wxc_h
#define wxc_h

/* eiffel uses stdcall but we use __cdecl!! */
#ifdef _stdcall
# undef _stdcall
#endif

#define _stdcall
#define EXPORT

/*-----------------------------------------------------------------------------
  Standard includes
-----------------------------------------------------------------------------*/
#include "wxc_types.h"
#include "wxc_glue.h"


/*-----------------------------------------------------------------------------
  Modular extra exports
-----------------------------------------------------------------------------*/
#include "db.h"
#include "dragimage.h"
#include "graphicscontext.h"
#include "glcanvas.h"
#include "sound.h"
#include "managed.h"
#include "mediactrl.h"
#include "previewframe.h"
#include "printout.h"
#include "textstream.h"
#include "stc.h"

/*-----------------------------------------------------------------------------
  Extra exports
-----------------------------------------------------------------------------*/

/* wxClosure */
TClassDefExtend(wxClosure,wxObject)
TClass(wxClosure)  wxClosure_Create( TClosureFun _fun_CEvent, void* _data );
void*              wxClosure_GetData( TSelf(wxClosure) _obj );

TClass(wxClosure)  wxEvtHandler_GetClosure( TSelf(wxEvtHandler) _obj, int id, int type );

/** Get the client data in the form of a closure. Use 'closureGetData' to get to the actual data.*/
TClass(wxClosure)  wxEvtHandler_GetClientClosure( TSelf(wxEvtHandler) _obj );
/** Set the client data as a closure. The closure data contains the data while the function is called on deletion. */
void               wxEvtHandler_SetClientClosure( TSelf(wxEvtHandler) _obj, TClass(wxClosure) closure );

/** Get the reference data of an object as a closure: only works if properly initialized. Use 'closureGetData' to get to the actual data. */
TClass(wxClosure)  wxObject_GetClientClosure( TSelf(wxObject) _obj );
/** Set the reference data of an object as a closure. The closure data contains the data while the function is called on deletion. Returns 'True' on success. Only works if the reference data is unused by wxWindows! */
void               wxObject_SetClientClosure( TSelf(wxObject) _obj, TClass(wxClosure) closure );

/* extra class definitions for classInfo */
TClassDefExtend(wxGauge95,wxGauge)
TClassDefExtend(wxGaugeMSW,wxGauge)
TClassDefExtend(wxSlider95,wxSlider)
TClassDefExtend(wxSliderMSW,wxSlider)


/* Object */
void wxObject_Delete( TSelf(wxObject) obj );

/* Frame */
TStringLen  wxFrame_GetTitle( TSelf(wxFrame) _obj, TStringOutVoid _buf );
void        wxFrame_SetTitle( TSelf(wxFrame) _frame, TString _txt );
TBool       wxFrame_SetShape( TSelf(wxFrame) self, TClass(wxRegion) region);
TBool       wxFrame_ShowFullScreen( TSelf(wxFrame) self, TBool show, int style);
TBool       wxFrame_IsFullScreen( TSelf(wxFrame) self );
void        wxFrame_Centre( TSelf(wxFrame) self, int orientation );

/* Create/Delete */
void   wxCursor_Delete( TSelf(wxCursor) _obj );
void  wxDateTime_Delete(TSelf(wxDateTime) _obj);

/* wxMouseEvent */
int   wxMouseEvent_GetWheelDelta( TSelf(wxMouseEvent) _obj );
int   wxMouseEvent_GetWheelRotation( TSelf(wxMouseEvent) _obj );
int   wxMouseEvent_GetButton( TSelf(wxMouseEvent) _obj );
int   expEVT_MOUSEWHEEL(  );

void wxcGetMousePosition( TPointOut(x,y) );


/* wxDC */
double wxDC_GetUserScaleX( TSelf(wxDC) dc );
double wxDC_GetUserScaleY( TSelf(wxDC) dc );

/* wxWindow */
void  wxWindow_ConvertDialogToPixelsEx( TSelf(wxWindow) _obj, TPoint(x,y), TPointOut(_x,_y) );
void  wxWindow_ConvertPixelsToDialogEx( TSelf(wxWindow) _obj, TPoint(x,y), TPointOut(_x,_y) );
void  wxWindow_ScreenToClient2(TSelf(wxWindow) self, TPoint(x,y), TPointOut(cx,cy));

/* wxString helpers */
TClass(wxString) wxString_Create( TString buffer );
TClass(wxString) wxString_CreateLen( TString buffer, int len );
void             wxString_Delete( TSelf(wxString) s );
TStringLen       wxString_GetString( TSelf(wxString) s, TStringOut buffer );


/* menu */
TClass(wxMenuBar) wxMenu_GetMenuBar( TSelf(wxMenu) _obj );
TClass(wxFrame)   wxMenuBar_GetFrame( TSelf(wxMenuBar) _obj );

/* listctrl */
int expEVT_SORT();
int expEVT_COMMAND_LIST_CACHE_HINT();
int expEVT_COMMAND_LIST_COL_RIGHT_CLICK();
int expEVT_COMMAND_LIST_COL_BEGIN_DRAG();
int expEVT_COMMAND_LIST_COL_DRAGGING();
int expEVT_COMMAND_LIST_COL_END_DRAG();

int wxListEvent_GetCacheFrom( TSelf(wxListEvent) _obj);
int wxListEvent_GetCacheTo( TSelf(wxListEvent) _obj);

void wxListCtrl_AssignImageList( TSelf(wxListCtrl) _obj, TClass(wxImageList) images, int which );
void wxListCtrl_GetColumn2( TSelf(wxListCtrl) _obj, int col, TClassRef(wxListItem) item);
void wxListCtrl_GetItem2( TSelf(wxListCtrl) _obj, TClassRef(wxListItem) info);
void wxListCtrl_GetItemPosition2( TSelf(wxListCtrl) _obj, int item, TPointOut(x,y));
/** Sort items in a list control. Takes a closure that is called with a 'CommandEvent' where the @Int@ is the item data of the first item and the @ExtraLong@ the item data of the second item. The event handler should set the @Int@ to 0 when the items are equal, -1 when the first is less, and 1 when the second is less. */
TBoolInt wxListCtrl_SortItems2(TSelf(wxListCtrl) _obj, TClass(wxClosure) closure );

/* tree ctrl */
TClassDefExtend(wxcTreeItemData,wxTreeItemData)

/** Create tree item data with a closure. The closure data contains the data while the function is called on deletion. */
TClass(wxcTreeItemData) wxcTreeItemData_Create( TClass(wxClosure) closure );
/** Get the client data in the form of a closure. Use 'closureGetData' to get to the actual data.*/
TClass(wxClosure) wxcTreeItemData_GetClientClosure( TSelf(wxcTreeItemData) self );
/** Set the tree item data with a closure. The closure data contains the data while the function is called on deletion. */
void  wxcTreeItemData_SetClientClosure( TSelf(wxcTreeItemData) self, TClass(wxClosure) closure );

TClass(wxTreeItemId) wxTreeItemId_Clone( TSelf(wxTreeItemId) _obj);
TClass(wxTreeItemId) wxTreeItemId_CreateFromValue(int value);
int wxTreeItemId_GetValue( TSelf(wxTreeItemId) _obj);


TClass(wxKeyEvent) wxTreeEvent_GetKeyEvent( TSelf(wxTreeEvent) _obj);
int    wxTreeEvent_IsEditCancelled( TSelf(wxTreeEvent) _obj);
void   wxTreeEvent_Allow( TSelf(wxTreeEvent) _obj);

TClass(wxTreeCtrl) wxTreeCtrl_Create2( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );
void   wxTreeCtrl_InsertItem2( TSelf(wxTreeCtrl) _obj, TClass(wxWindow) parent, TClass(wxTreeItemId) idPrevious, TStringVoid text, int image, int selectedImage, TClass(wxClosure) closure, TClassRef(wxTreeItemId) _item );
void   wxTreeCtrl_InsertItemByIndex2( TSelf(wxTreeCtrl) _obj, TClass(wxWindow) parent, int index, TStringVoid text, int image, int selectedImage, TClass(wxClosure) closure, TClassRef(wxTreeItemId) _item );
TClass(wxClosure)  wxTreeCtrl_GetItemClientClosure( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );
void   wxTreeCtrl_SetItemClientClosure( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClass(wxClosure) closure );
void   wxTreeCtrl_AssignImageList(TSelf(wxTreeCtrl) _obj, TClass(wxImageList) imageList );
void   wxTreeCtrl_AssignStateImageList(TSelf(wxTreeCtrl) _obj, TClass(wxImageList) imageList );


/* dc */
/** Get the color of pixel. Note: this is not a portable method at the moment and its use is discouraged. */
void wxDC_GetPixel2( TSelf(wxDC) _obj, TPoint(x,y), TClassRef(wxColour) col);


/* scrolledwindow */
void wxScrolledWindow_SetScrollRate( TSelf(wxScrolledWindow) _obj, int xstep, int ystep );


/* wxObject */
TClassDef(wxObject)
TClass(wxClassInfo)  wxObject_GetClassInfo( TSelf(wxObject) _obj );
TBool       wxObject_IsKindOf( TSelf(wxObject) _obj, TClass(wxClassInfo) classInfo );
TBool       wxObject_IsScrolledWindow( TSelf(wxObject) _obj );


/* wxClassInfo */
TClassDef(wxClassInfo)
TClass(wxClassInfo)  wxClassInfo_FindClass( TString _txt );
TStringLen  wxClassInfo_GetBaseClassName1( TSelf(wxClassInfo) _obj, TStringOutVoid _buf );
TStringLen  wxClassInfo_GetBaseClassName2( TSelf(wxClassInfo) _obj, TStringOutVoid _buf );
TStringLen  wxClassInfo_GetClassNameEx( TSelf(wxClassInfo) _obj, TStringOutVoid _buf );
int         wxClassInfo_GetSize( TSelf(wxClassInfo) _obj );
TBool       wxClassInfo_IsKindOfEx( TSelf(wxClassInfo) _obj, TClass(wxClassInfo) classInfo );

/* wxNotebook */
void        wxNotebook_AssignImageList( TSelf(wxNotebook) _obj, TClass(wxImageList) imageList );

/* Timers */
TClassDefExtend(wxTimerEx,wxTimer)
void               wxTimerEx_Connect( TSelf(wxTimerEx) _obj, TClass(wxClosure) closure );
TClass(wxTimerEx)  wxTimerEx_Create(  );
TClass(wxClosure)  wxTimerEx_GetClosure( TSelf(wxTimerEx) _obj );

/* Menu */
void  wxMenu_AppendRadioItem( TSelf(wxMenu) self, int id, TString text, TString help);


/* Menu Item */
TClass(wxMenuItem)  wxMenuItem_CreateSeparator();
TClass(wxMenuItem)  wxMenuItem_CreateEx(int id, TString label, TString help, int itemkind, TClass(wxMenu) submenu);

/* Toolbar */
void wxToolBar_AddTool2( TSelf(wxToolBar) _obj, int toolId, TString label, TClass(wxBitmap) bmp, TClass(wxBitmap) bmpDisabled, int itemKind, TString shortHelp, TString longHelp );

/* Progress dialog */
TClass(wxProgressDialog) wxProgressDialog_Create( TString title, TString message, int max, TClass(wxWindow) parent, int style );
int  wxProgressDialog_Update(TSelf(wxProgressDialog) obj, int value );
int  wxProgressDialog_UpdateWithMessage( TSelf(wxProgressDialog) obj, int value, TString message );
void wxProgressDialog_Resume( TSelf(wxProgressDialog) obj );

/** Get the version number of wxWindows as a number composed of the major version times 1000, minor version times 100, and the release number. For example, release 2.1.15 becomes 2115. */
int wxVersionNumber();
/** Check if a preprocessor macro is defined. For example, @wxIsDefined("__WXGTK__")@ or @wxIsDefined("wxUSE_GIF")@. */
TBoolInt wxIsDefined( TString s );


/* new Events */
int expEVT_DELETE();
int expEVT_HTML_CELL_CLICKED();
int expEVT_HTML_CELL_MOUSE_HOVER();
int expEVT_HTML_LINK_CLICKED();
int expEVT_HTML_SET_TITLE();
int expEVT_INPUT_SINK();

/* Input sink */
TClassDefExtend(wxInputSink,wxThread)

/** Create an event driven input stream. It is unsafe to reference the original inputStream after this call! The last parameter @bufferLen@ gives the default input batch size. The sink is automatically destroyed whenever the input stream has no more input. */
TClass(wxInputSink) wxInputSink_Create( TClass(wxInputStream) input, TClass(wxEvtHandler) evtHandler, int bufferLen );
/** After creation, retrieve the @id@ of the sink to connect to @wxEVT_INPUT_SINK@ events. */
int   wxInputSink_GetId( TSelf(wxInputSink) obj );
/** After event connection, start non-blocking reading of the inputstream. This will generate @inputSinkEvent@ events. */
void  wxInputSink_Start( TSelf(wxInputSink) obj );

/* Input sink events */
TClassDefExtend(wxInputSinkEvent,wxEvent)

/** Get the input status (@wxSTREAM_NO_ERROR@ is ok). */
int wxInputSinkEvent_LastError( TSelf(wxInputSinkEvent) obj );
/** The number of characters in the input buffer. */
int wxInputSinkEvent_LastRead( TSelf(wxInputSinkEvent) obj );
/** The input buffer. */
char* wxInputSinkEvent_LastInput( TSelf(wxInputSinkEvent) obj );


/* html events */
TClassDefExtend(wxcHtmlEvent,wxCommandEvent)

TClass(wxMouseEvent) wxcHtmlEvent_GetMouseEvent( TSelf(wxcHtmlEvent) self );
TClass(wxHtmlCell)   wxcHtmlEvent_GetHtmlCell( TSelf(wxcHtmlEvent) self );
/** Return the /id/ attribute of the associated html cell (if applicable) */
TClass(wxString)     wxcHtmlEvent_GetHtmlCellId( TSelf(wxcHtmlEvent) self );
/** Return the /href/ attribute of the associated html anchor (if applicable) */
TClass(wxString)     wxcHtmlEvent_GetHref( TSelf(wxcHtmlEvent) self );
TClass(wxString)     wxcHtmlEvent_GetTarget( TSelf(wxcHtmlEvent) self );
void                 wxcHtmlEvent_GetLogicalPosition( TSelf(wxcHtmlEvent) self, TPointOut(x,y) );

/* html window */
TClassDefExtend(wxcHtmlWindow,wxHtmlWindow)
TClass(wxcHtmlWindow) wxcHtmlWindow_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl, TString _txt );

TClass(wxHtmlWindow) wxHtmlWindow_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl, TString _txt );
TBool                wxHtmlWindow_AppendToPage( TSelf(wxHtmlWindow) _obj, TString source );
TClass(wxHtmlContainerCell) wxHtmlWindow_GetInternalRepresentation( TSelf(wxHtmlWindow) _obj );
TStringLen           wxHtmlWindow_GetOpenedAnchor( TSelf(wxHtmlWindow) _obj, TStringOutVoid _buf ) ;
TStringLen           wxHtmlWindow_GetOpenedPage( TSelf(wxHtmlWindow) _obj, TStringOutVoid _buf );
TStringLen            wxHtmlWindow_GetOpenedPageTitle( TSelf(wxHtmlWindow) _obj, TStringOutVoid _buf );
TClass(wxFrame)      wxHtmlWindow_GetRelatedFrame( TSelf(wxHtmlWindow) _obj );
TBool                wxHtmlWindow_HistoryBack( TSelf(wxHtmlWindow) _obj);
TBool                wxHtmlWindow_HistoryCanBack( TSelf(wxHtmlWindow) _obj );
TBool                wxHtmlWindow_HistoryCanForward( TSelf(wxHtmlWindow) _obj );
void                 wxHtmlWindow_HistoryClear( TSelf(wxHtmlWindow) _obj);
TBool                wxHtmlWindow_HistoryForward( TSelf(wxHtmlWindow) _obj );
TBool                wxHtmlWindow_LoadPage( TSelf(wxHtmlWindow) _obj, TString location );
void                 wxHtmlWindow_ReadCustomization( TSelf(wxHtmlWindow) _obj, TClass(wxConfigBase) cfg, TString path);
void                 wxHtmlWindow_SetBorders( TSelf(wxHtmlWindow) _obj, int b );
void                 wxHtmlWindow_SetFonts( TSelf(wxHtmlWindow) _obj, TString normal_face, TString fixed_face, int * sizes );
void                 wxHtmlWindow_SetPage( TSelf(wxHtmlWindow) _obj, TString source );
void                 wxHtmlWindow_SetRelatedFrame( TSelf(wxHtmlWindow) _obj , TClass(wxFrame) frame, TString format );
void                 wxHtmlWindow_SetRelatedStatusBar( TSelf(wxHtmlWindow) _obj, int bar);
void                 wxHtmlWindow_WriteCustomization( TSelf(wxHtmlWindow) _obj, TClass(wxConfigBase) cfg, TString path );

/* wxGridCellTextEnterEditor */
TClassDefExtend(wxGridCellTextEnterEditor,wxGridCellTextEditor)
TClass(wxGridCellTextEnterEditor) wxGridCellTextEnterEditor_Ctor();

/* logger */
TClass(wxLogStderr)   wxLogStderr_Create();
TClass(wxLogStderr)   wxLogStderr_CreateStdOut();
TClass(wxLogNull)     wxLogNull_Create();
TClass(wxLogTextCtrl) wxLogTextCtrl_Create( TClass(wxTextCtrl) text );
TClass(wxLogWindow)   wxLogWindow_Create( TClass(wxWindow) parent, TString title, TBoolInt showit, TBoolInt passthrough );
TClass(wxFrame)       wxLogWindow_GetFrame( TSelf(wxLogWindow) obj );

void   LogError(TStringVoid _msg);
void   LogFatalError(TStringVoid _msg);
void   LogWarning(TStringVoid _msg);
void   LogMessage(TStringVoid _msg);
void   LogVerbose(TStringVoid _msg);
void   LogStatus(TStringVoid _msg);
void   LogSysError(TStringVoid _msg);
void   LogDebug(TStringVoid _msg);
void   LogTrace(TStringVoid mask, TStringVoid _msg);

void       wxLog_AddTraceMask( TSelf(wxLog) _obj, TStringVoid str );
void       wxLog_Delete( TSelf(wxLog) _obj );
void       wxLog_DontCreateOnDemand( TSelf(wxLog) _obj );
void       wxLog_Flush( TSelf(wxLog) _obj );
void       wxLog_FlushActive( TSelf(wxLog) _obj );
TClass(wxLog)  wxLog_GetActiveTarget(  );
char*      wxLog_GetTimestamp( TSelf(wxLog) _obj );
int        wxLog_GetTraceMask( TSelf(wxLog) _obj );
int        wxLog_GetVerbose( TSelf(wxLog) _obj );
TBool      wxLog_HasPendingMessages( TSelf(wxLog) _obj );
TBool      wxLog_IsAllowedTraceMask( TSelf(wxLog) _obj, TClass(wxMask) mask );
void       wxLog_OnLog( TSelf(wxLog) _obj, int level, TStringVoid szString, int t );
void       wxLog_RemoveTraceMask( TSelf(wxLog) _obj, TStringVoid str );
void       wxLog_Resume( TSelf(wxLog) _obj );
TClass(wxLog)  wxLog_SetActiveTarget( TSelf(wxLog) pLogger );
void       wxLog_SetTimestamp( TSelf(wxLog) _obj, TStringVoid ts );
void       wxLog_SetTraceMask( TSelf(wxLog) _obj, int ulMask );
void       wxLog_SetVerbose( TSelf(wxLog) _obj, TBoolInt bVerbose );
void       wxLog_Suspend( TSelf(wxLog) _obj );


/* process */
TClass(wxProcess) wxProcess_Open( TString cmd, int flags );
TBool      wxProcess_IsErrorAvailable( TSelf(wxProcess) _obj );
TBool      wxProcess_IsInputAvailable( TSelf(wxProcess) _obj );
TBool      wxProcess_IsInputOpened( TSelf(wxProcess) _obj );
int        wxKill( int pid, int signal );

void       wxStreamBase_Delete( TSelf(wxStreamBase) obj);

/* Dialogs */
void        wxGetColourFromUser(TClass(wxWindow) parent, TClass(wxColour) colInit, TClassRef(wxColour) colour );
void        wxGetFontFromUser(TClass(wxWindow) parent, TClass(wxFont) fontInit, TClassRef(wxFont) font );
TStringLen  wxGetPasswordFromUser(TString message, TString caption, TString defaultText, TClass(wxWindow) parent, TStringOut _buf );
TStringLen  wxGetTextFromUser(TString message, TString caption, TString defaultText, TClass(wxWindow) parent, TPoint(x,y), TBool center, TStringOut _buf );
long        wxGetNumberFromUser( TString message, TString prompt, TString caption, long value, long min, long max, TClass(wxWindow) parent, TPoint(x,y) );
void        wxcBell();
void        wxcBeginBusyCursor();
void        wxcEndBusyCursor();
void        wxcIsBusy();

/* text ctrl */
TBool               wxTextCtrl_EmulateKeyPress( TSelf(wxTextCtrl) _obj, TClass(wxKeyEvent) keyevent);
TClass (wxTextAttr) wxTextCtrl_GetDefaultStyle( TSelf(wxTextCtrl) _obj );
TStringLen          wxTextCtrl_GetRange( TSelf(wxTextCtrl) _obj, long from, long to, TStringOutVoid _buf );
TStringLen          wxTextCtrl_GetStringSelection( TSelf(wxTextCtrl) _obj, TStringOutVoid _buf );
TBool               wxTextCtrl_IsMultiLine( TSelf(wxTextCtrl) _obj );
TBool               wxTextCtrl_IsSingleLine( TSelf(wxTextCtrl) _obj );
TBool               wxTextCtrl_SetDefaultStyle( TSelf(wxTextCtrl) _obj, TClass(wxTextAttr) style);
void                wxTextCtrl_SetMaxLength( TSelf(wxTextCtrl) _obj, long len );
TBool               wxTextCtrl_SetStyle( TSelf(wxTextCtrl) _obj, long start, long end, TClass(wxTextAttr) style );

/* text attributes */
TClass(wxTextAttr)  wxTextAttr_Create(TClass(wxColour) colText, TClass(wxColour) colBack, TClass(wxFont) font);
TClass(wxTextAttr)  wxTextAttr_CreateDefault();
void      wxTextAttr_Delete( TSelf(wxTextAttr) _obj );
void      wxTextAttr_GetBackgroundColour( TSelf(wxTextAttr) _obj, TClassRef(wxColour) colour  );
void      wxTextAttr_GetFont( TSelf(wxTextAttr) _obj, TClassRef(wxFont) font );
void      wxTextAttr_GetTextColour( TSelf(wxTextAttr) _obj, TClassRef(wxColour) colour );
TBool     wxTextAttr_HasBackgroundColour( TSelf(wxTextAttr) _obj );
TBool     wxTextAttr_HasFont( TSelf(wxTextAttr) _obj );
TBool     wxTextAttr_HasTextColour( TSelf(wxTextAttr) _obj );
TBool     wxTextAttr_IsDefault( TSelf(wxTextAttr) _obj );
void      wxTextAttr_SetTextColour(TSelf(wxTextAttr) _obj, TClass(wxColour) colour );
void      wxTextAttr_SetBackgroundColour(TSelf(wxTextAttr) _obj, TClass(wxColour) colour );
void      wxTextAttr_SetFont(TSelf(wxTextAttr) _obj, TClass(wxFont) font );

/* ConfigBase */
TClassDefExtend(wxFileConfig,wxConfigBase)

TClass(wxConfigBase) wxConfigBase_Get();
void                 wxConfigBase_Set( TClass(wxConfigBase) self );
TClass(wxFileConfig) wxFileConfig_Create( TClass(wxInputStream) inp );

/* Image.cpp */
TClass(wxBitmap) wxBitmap_CreateFromImage( TClass(wxImage) image, int depth );
TClass(wxImage) wxImage_CreateFromDataEx( TSize(width,height), void* data, TBoolInt isStaticData);
void wxImage_Delete( TSelf(wxImage) image );

/** Create from rgb int. */
TClass(wxColour) wxColour_CreateFromInt(int rgb);
/** Return colors as an rgb int. */
int wxColour_GetInt( TSelf(wxColour) colour);
/** Create from rgba unsigned int. */
TClass(wxColour) wxColour_CreateFromUnsignedInt(TUInt rgba);
/** Return colors as an rgba unsigned int. */
TUInt wxColour_GetUnsignedInt( TSelf(wxColour) colour);

/** Create from system colour. */
TClass(wxColour) wxcSystemSettingsGetColour( int systemColour );


/* basic pixel manipulation */
void wxcSetPixelRGB( TUInt8* buffer, int width, TPoint(x,y), int rgb  );
int  wxcGetPixelRGB( TUInt8* buffer, int width, TPoint(x,y) );
void wxcSetPixelRowRGB( TUInt8* buffer, int width, TPoint(x,y), int rgbStart, int rgbEnd, int count );
void wxcInitPixelsRGB( TUInt8* buffer, TSize(width,height), int rgba );
void wxcSetPixelRGBA( TUInt8* buffer, int width, TPoint(x,y), TUInt rgba  );
int  wxcGetPixelRGBA( TUInt8* buffer, int width, TPoint(x,y) );
void wxcSetPixelRowRGBA( TUInt8* buffer, int width, TPoint(x,y), int rgbaStart, int rgbEnd, TUInt count );
void wxcInitPixelsRGBA( TUInt8* buffer, TSize(width,height), TUInt rgba );

/* malloc/free */
void* wxcMalloc(int size );
void  wxcFree( void* p );

/* wakeup idle */
void wxcWakeUpIdle();

/* application directory */
/** Return the directory of the application. On unix systems (except MacOS X), it is not always possible to determine this correctly. Therefore, the APPDIR environment variable is returned first if it is defined. */
TStringLen wxGetApplicationDir( TStringOut buffer);
/** Return the full path of the application. On unix systems (except MacOS X), it is not always possible to determine this correctly. */
TStringLen wxGetApplicationPath( TStringOut buffer);

/* ELJApp */
void  ELJApp_InitializeC( TClass(wxClosure) closure, int _argc, TChar** _argv );
int   ELJApp_GetIdleInterval();
void  ELJApp_SetIdleInterval( int interval );


#endif /* wxc_h */
