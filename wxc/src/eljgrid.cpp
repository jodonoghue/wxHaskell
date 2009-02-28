#include "wrapper.h"
#include "eljgrid.h"

extern "C"
{

EWXWEXPORT(wxGridCellCoordsArray*,wxGridCellCoordsArray_Create)()
{
	return new wxGridCellCoordsArray();
}

EWXWEXPORT(void,wxGridCellCoordsArray_Delete)(wxGridCellCoordsArray* self)
{
	delete self;
}

EWXWEXPORT(int,wxGridCellCoordsArray_GetCount)(wxGridCellCoordsArray* self)
{
	return self->GetCount();
}

EWXWEXPORT(void,wxGridCellCoordsArray_Item)(void* _obj,int _idx,int* _c,int* _r)
{
	*_c = ((wxGridCellCoordsArray*)_obj)->Item(_idx).GetCol();
	*_r = ((wxGridCellCoordsArray*)_obj)->Item(_idx).GetRow();
}


EWXWEXPORT(bool,wxGridCellEditor_IsCreated)(wxGridCellEditor* self)
{
	return self->IsCreated();
}
	
EWXWEXPORT(void*,wxGridCellEditor_GetControl)(void* _obj)
{
	return (void*)((wxGridCellEditor*)_obj)->GetControl();
}
	
EWXWEXPORT(void,wxGridCellEditor_SetControl)(void* _obj,void* control)
{
	((wxGridCellEditor*)_obj)->SetControl((wxControl*) control);
}
	
EWXWEXPORT(void,wxGridCellEditor_Create)(void* _obj,wxWindow* parent,int id,void* evtHandler)
{
	((wxGridCellEditor*)_obj)->Create(parent, (wxWindowID) id, (wxEvtHandler*) evtHandler);
}
	
EWXWEXPORT(void,wxGridCellEditor_SetSize)(void* _obj,int x,int y,int w,int h)
{
	((wxGridCellEditor*)_obj)->SetSize(wxRect(x, y, w, h));
}
	
EWXWEXPORT(void,wxGridCellEditor_Show)(wxGridCellEditor* self,bool show,void* attr)
{
	self->Show(show, (wxGridCellAttr*)attr);
}
	
EWXWEXPORT(void,wxGridCellEditor_PaintBackground)(void* _obj,int x,int y,int w,int h,void* attr)
{
	((wxGridCellEditor*)_obj)->PaintBackground(wxRect(x, y, w, h), (wxGridCellAttr*)attr);
}
	
EWXWEXPORT(void,wxGridCellEditor_BeginEdit)(void* _obj,int row,int col,void* grid)
{
	((wxGridCellEditor*)_obj)->BeginEdit(row, col, (wxGrid*)grid);
}
	
EWXWEXPORT(bool,wxGridCellEditor_EndEdit)(wxGridCellEditor* self,int row,int col,wxGrid* grid)
{
	return self->EndEdit(row, col,  grid);
}
	
EWXWEXPORT(void,wxGridCellEditor_Reset)(void* _obj)
{
	((wxGridCellEditor*)_obj)->Reset();
}
	
EWXWEXPORT(bool,wxGridCellEditor_IsAcceptedKey)(wxGridCellEditor* self,wxKeyEvent* event)
{
	return self->IsAcceptedKey(*event);
}
	
EWXWEXPORT(void,wxGridCellEditor_StartingKey)(void* _obj,void* event)
{
	((wxGridCellEditor*)_obj)->StartingKey(*((wxKeyEvent*)event));
}
	
EWXWEXPORT(void,wxGridCellEditor_StartingClick)(void* _obj)
{
	((wxGridCellEditor*)_obj)->StartingClick();
}
	
EWXWEXPORT(void,wxGridCellEditor_HandleReturn)(void* _obj,void* event)
{
	((wxGridCellEditor*)_obj)->HandleReturn(*((wxKeyEvent*)event));
}
	
EWXWEXPORT(void,wxGridCellEditor_Destroy)(void* _obj)
{
	((wxGridCellEditor*)_obj)->Destroy();
}
	
EWXWEXPORT(void,wxGridCellEditor_SetParameters)(void* _obj,wxString* params)
{
	((wxGridCellEditor*)_obj)->SetParameters(*params);
}
	
EWXWEXPORT(void*,wxGridCellTextEditor_Ctor)()
{
	return (void*) new wxGridCellTextEditor();
}

EWXWEXPORT(void*,wxGridCellNumberEditor_Ctor)(int min,int max)
{
	return (void*) new wxGridCellNumberEditor(min, max);
}

EWXWEXPORT(void*,wxGridCellFloatEditor_Ctor)(int width,int precision)
{
	return (void*) new wxGridCellFloatEditor(width, precision);
}

EWXWEXPORT(void*,wxGridCellBoolEditor_Ctor)()
{
	return (void*) new wxGridCellBoolEditor();
}

EWXWEXPORT(void*,wxGridCellChoiceEditor_Ctor)(int count,void* choices,bool allowOthers)
{
	wxString items[256];

	for (int i = 0; i < count; i++)
		items[i] = ((wxChar**)choices)[i];

	return (void*) new wxGridCellChoiceEditor (count, items, allowOthers);
}

EWXWEXPORT(void*,wxGridCellAttr_Ctor)()
{
	return (void*) new wxGridCellAttr();
}

EWXWEXPORT(void,wxGridCellAttr_IncRef)(void* _obj)
{
	((wxGridCellAttr*)_obj)->IncRef();
}
	
EWXWEXPORT(void,wxGridCellAttr_DecRef)(void* _obj)
{
	((wxGridCellAttr*)_obj)->DecRef();
}
	
EWXWEXPORT(void,wxGridCellAttr_SetTextColour)(void* _obj,void* colText)
{
	((wxGridCellAttr*)_obj)->SetTextColour(*((wxColour*)colText));
}
	
EWXWEXPORT(void,wxGridCellAttr_SetBackgroundColour)(void* _obj,void* colBack)
{
	((wxGridCellAttr*)_obj)->SetBackgroundColour(*((wxColour*)colBack));
}
	
EWXWEXPORT(void,wxGridCellAttr_SetFont)(void* _obj,void* font)
{
	((wxGridCellAttr*)_obj)->SetFont(*((wxFont*)font));
}
	
EWXWEXPORT(void,wxGridCellAttr_SetAlignment)(void* _obj,int hAlign,int vAlign)
{
	((wxGridCellAttr*)_obj)->SetAlignment(hAlign, vAlign);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetReadOnly)(void* _obj,bool isReadOnly)
{
	((wxGridCellAttr*)_obj)->SetReadOnly(isReadOnly);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetRenderer)(void* _obj,void* renderer)
{
	((wxGridCellAttr*)_obj)->SetRenderer((wxGridCellRenderer*)renderer);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetEditor)(void* _obj,void* editor)
{
	((wxGridCellAttr*)_obj)->SetEditor((wxGridCellEditor*) editor);
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasTextColour)(wxGridCellAttr* self)
{
	return self->HasTextColour();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasBackgroundColour)(wxGridCellAttr* self)
{
	return self->HasBackgroundColour();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasFont)(wxGridCellAttr* self)
{
	return self->HasFont();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasAlignment)(wxGridCellAttr* self)
{
	return self->HasAlignment();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasRenderer)(wxGridCellAttr* self)
{
	return self->HasRenderer();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasEditor)(wxGridCellAttr* self)
{
	return self->HasEditor();
}
	
EWXWEXPORT(void,wxGridCellAttr_GetTextColour)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGridCellAttr*)_obj)->GetTextColour();
}
	
EWXWEXPORT(void,wxGridCellAttr_GetBackgroundColour)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGridCellAttr*)_obj)->GetBackgroundColour();
}
	
EWXWEXPORT(void,wxGridCellAttr_GetFont)(void* _obj,void* font)
{
	*((wxFont*)font) = ((wxGridCellAttr*)_obj)->GetFont();
}
	
EWXWEXPORT(void,wxGridCellAttr_GetAlignment)(void* _obj,int* hAlign,int* vAlign)
{
	((wxGridCellAttr*)_obj)->GetAlignment(hAlign, vAlign);
}
	
EWXWEXPORT(void*,wxGridCellAttr_GetRenderer)(void* _obj,void* grid,int row,int col)
{
	return (void*)((wxGridCellAttr*)_obj)->GetRenderer((wxGrid*)grid, row, col);
}
	
EWXWEXPORT(void*,wxGridCellAttr_GetEditor)(void* _obj,void* grid,int row,int col)
{
	return (void*)((wxGridCellAttr*)_obj)->GetEditor((wxGrid*)grid, row, col);
}
	
EWXWEXPORT(bool,wxGridCellAttr_IsReadOnly)(wxGridCellAttr* self)
{
	return self->IsReadOnly();
}
	
EWXWEXPORT(void,wxGridCellAttr_SetDefAttr)(void* _obj,void* defAttr)
{
	((wxGridCellAttr*)_obj)->SetDefAttr((wxGridCellAttr*) defAttr);
}
	
EWXWEXPORT(void*,wxGrid_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxGrid (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl | wxWANTS_CHARS);
}

EWXWEXPORT(void,wxGrid_CreateGrid)(void* _obj,int rows,int cols,int selmode)
{
	((wxGrid*)_obj)->CreateGrid (rows, cols, (wxGrid::wxGridSelectionModes)selmode);
}

EWXWEXPORT(void,wxGrid_SetSelectionMode)(void* _obj,int selmode)
{
	((wxGrid*)_obj)->SetSelectionMode((wxGrid::wxGridSelectionModes) selmode);
}
	
EWXWEXPORT(int,wxGrid_GetNumberRows)(void* _obj)
{
	return ((wxGrid*)_obj)->GetNumberRows();
}
	
EWXWEXPORT(int,wxGrid_GetNumberCols)(void* _obj)
{
	return ((wxGrid*)_obj)->GetNumberCols();
}
	
EWXWEXPORT(void,wxGrid_CalcRowLabelsExposed)(void* _obj,void* reg)
{
	((wxGrid*)_obj)->CalcRowLabelsExposed(*((wxRegion*) reg));
}
	
EWXWEXPORT(void,wxGrid_CalcColLabelsExposed)(void* _obj,void* reg)
{
	((wxGrid*)_obj)->CalcColLabelsExposed(*((wxRegion*) reg));
}
	
EWXWEXPORT(void,wxGrid_CalcCellsExposed)(void* _obj,void* reg)
{
	((wxGrid*)_obj)->CalcCellsExposed(*((wxRegion*)reg));
}
	
EWXWEXPORT(void,wxGrid_NewCalcCellsExposed)(void* _obj,void* reg,void* arr)
{
#if wxVERSION_NUMBER >= 2400
	*((wxGridCellCoordsArray*)arr) = ((wxGrid*)_obj)->CalcCellsExposed(*((wxRegion*)reg));
#endif
}
	
EWXWEXPORT(void,wxGrid_ProcessRowLabelMouseEvent)(void* _obj,void* event)
{
	((wxGrid*)_obj)->ProcessRowLabelMouseEvent(*((wxMouseEvent*)event));
}
	
EWXWEXPORT(void,wxGrid_ProcessColLabelMouseEvent)(void* _obj,void* event)
{
	((wxGrid*)_obj)->ProcessColLabelMouseEvent(*((wxMouseEvent*)event));
}
	
EWXWEXPORT(void,wxGrid_ProcessCornerLabelMouseEvent)(void* _obj,void* event)
{
	((wxGrid*)_obj)->ProcessCornerLabelMouseEvent(*((wxMouseEvent*) event));
}
	
EWXWEXPORT(void,wxGrid_ProcessGridCellMouseEvent)(void* _obj,void* event)
{
	((wxGrid*)_obj)->ProcessGridCellMouseEvent(*((wxMouseEvent*)event));
}
	
EWXWEXPORT(bool,wxGrid_ProcessTableMessage)(wxGrid* self,wxGridTableMessage* evt)
{
	return self->ProcessTableMessage(*evt);
}
	
EWXWEXPORT(void,wxGrid_DoEndDragResizeRow)(void* _obj)
{
	((wxGrid*)_obj)->DoEndDragResizeRow();
}
	
EWXWEXPORT(void,wxGrid_DoEndDragResizeCol)(void* _obj)
{
	((wxGrid*)_obj)->DoEndDragResizeCol();
}
	
EWXWEXPORT(void*,wxGrid_GetTable)(void* _obj)
{
	return (void*)((wxGrid*)_obj)->GetTable();
}
	
EWXWEXPORT(bool,wxGrid_SetTable)(wxGrid* self,wxGridTableBase* table,bool takeOwnership,int selmode)
{
	return self->SetTable(table, takeOwnership , (wxGrid::wxGridSelectionModes) selmode);
}
	
EWXWEXPORT(void,wxGrid_ClearGrid)(void* self)
{
	((wxGrid*)self)->ClearGrid();
}
	
EWXWEXPORT(bool,wxGrid_InsertRows)(wxGrid* self,int pos,int numRows,bool updateLabels)
{
	return self->InsertRows(pos, numRows, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_AppendRows)(wxGrid* self,int numRows,bool updateLabels)
{
	return self->AppendRows(numRows, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_DeleteRows)(wxGrid* self,int pos,int numRows,bool updateLabels)
{
	return self->DeleteRows(pos, numRows, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_InsertCols)(wxGrid* self,int pos,int numCols,bool updateLabels)
{
	return self->InsertCols(pos, numCols, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_AppendCols)(wxGrid* self,int numCols,int updateLabels)
{
	return self->AppendCols( numCols, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_DeleteCols)(wxGrid* self,int pos,int numCols,bool updateLabels)
{
	return self->DeleteCols(pos, numCols, updateLabels);
}
	
EWXWEXPORT(void,wxGrid_DrawGridCellArea)(void* _obj,void* dc)
{
#if wxVERSION_NUMBER >= 2400
	wxGridCellCoordsArray arr;
	((wxGrid*)_obj)->DrawGridCellArea(*((wxDC*) dc), arr);
#else
	((wxGrid*)_obj)->DrawGridCellArea(*((wxDC*) dc));
#endif
}
	
EWXWEXPORT(void,wxGrid_NewDrawGridCellArea)(void* _obj,void* dc,void* arr)
{
#if wxVERSION_NUMBER >= 2400
	((wxGrid*)_obj)->DrawGridCellArea(*((wxDC*) dc), *((wxGridCellCoordsArray*)arr));
#endif
}
	
EWXWEXPORT(void,wxGrid_DrawGridSpace)(void* _obj,void* dc)
{
	((wxGrid*)_obj)->DrawGridSpace(*((wxDC*) dc));
}
	
EWXWEXPORT(void,wxGrid_DrawCellBorder)(void* _obj,void* dc,int _row,int _col)
{
	((wxGrid*)_obj)->DrawCellBorder(*((wxDC*) dc), wxGridCellCoords(_row, _col));
}
	
EWXWEXPORT(void,wxGrid_DrawAllGridLines)(void* _obj,void* dc,void* reg)
{
	((wxGrid*)_obj)->DrawAllGridLines(*((wxDC*) dc), *((wxRegion*) reg));
}
	
EWXWEXPORT(void,wxGrid_DrawCell)(void* _obj,void* dc,int _row,int _col)
{
	((wxGrid*)_obj)->DrawCell(*((wxDC*) dc), wxGridCellCoords(_row, _col));
}
	
EWXWEXPORT(void,wxGrid_DrawHighlight)(void* _obj,void* dc)
{
#if wxVERSION_NUMBER >= 2400
	wxGridCellCoordsArray arr;
	((wxGrid*)_obj)->DrawHighlight(*((wxDC*) dc), arr);
#else
	((wxGrid*)_obj)->DrawHighlight(*((wxDC*) dc));
#endif
}
	
EWXWEXPORT(void,wxGrid_NewDrawHighlight)(void* _obj,void* dc,void* arr)
{
#if wxVERSION_NUMBER >= 2400
	((wxGrid*)_obj)->DrawHighlight(*((wxDC*) dc), *((wxGridCellCoordsArray*)arr));
#endif
}
	
EWXWEXPORT(void,wxGrid_DrawCellHighlight)(void* _obj,void* dc,void* attr)
{
	((wxGrid*)_obj)->DrawCellHighlight(*((wxDC*) dc), (const wxGridCellAttr*) attr);
}
	
EWXWEXPORT(void,wxGrid_DrawRowLabels)(void* _obj,void* dc)
{
#if wxVERSION_NUMBER >= 2400
	wxArrayInt arr;
	((wxGrid*)_obj)->DrawRowLabels(*((wxDC*) dc), arr);
#else
	((wxGrid*)_obj)->DrawRowLabels(*((wxDC*) dc));
#endif
}
	
EWXWEXPORT(void,wxGrid_DrawRowLabel)(void* _obj,void* dc,int row)
{
	((wxGrid*)_obj)->DrawRowLabel(*((wxDC*) dc), row);
}
	
EWXWEXPORT(void,wxGrid_DrawColLabels)(void* _obj,void* dc)
{
#if wxVERSION_NUMBER >= 2400
	wxArrayInt arr;
	((wxGrid*)_obj)->DrawColLabels(*((wxDC*) dc), arr);
#else
	((wxGrid*)_obj)->DrawColLabels(*((wxDC*) dc));
#endif
}
	
EWXWEXPORT(void,wxGrid_DrawColLabel)(void* _obj,void* dc,int col)
{
	((wxGrid*)_obj)->DrawColLabel(*((wxDC*) dc), col);
}
	
EWXWEXPORT(void,wxGrid_DrawTextRectangle)(void* _obj,void* dc,wxString* txt,int x,int y,int w,int h,int horizontalAlignment,int verticalAlignment)
{
	((wxGrid*)_obj)->DrawTextRectangle(*((wxDC*) dc), *txt, wxRect(x, y, w, h), horizontalAlignment, verticalAlignment);
}
	
EWXWEXPORT(int,wxGrid_StringToLines)(void* _obj,wxString* value,void* lines)
{
	int result = 0;
	wxArrayString arr;
	
	((wxGrid*)_obj)->StringToLines(*value, arr);
	
	result = arr.GetCount();
	
	if (lines)
	{
		for (int i = 0; i < result; i++)
			((const wxChar**)lines)[i] = wxStrdup (arr[i].c_str());
	}
	return result;
}
	
EWXWEXPORT(void,wxGrid_GetTextBoxSize)(void* _obj,void* dc,int count,void* lines,void* width,void* height)
{
	wxArrayString arr;

	for (int i = 0; i < count; i++)
		arr[i] = ((wxChar**)lines)[i];

	((wxGrid*)_obj)->GetTextBoxSize(*((wxDC*) dc), arr, (long*)width, (long*)height);
}
	
EWXWEXPORT(void,wxGrid_BeginBatch)(void* _obj)
{
	((wxGrid*)_obj)->BeginBatch();
}
	
EWXWEXPORT(void,wxGrid_EndBatch)(void* _obj)
{
	((wxGrid*)_obj)->EndBatch();
}
	
EWXWEXPORT(int,wxGrid_GetBatchCount)(void* _obj)
{
	return ((wxGrid*)_obj)->GetBatchCount();
}
	
EWXWEXPORT(bool,wxGrid_IsEditable)(wxGrid* self)
{
	return self->IsEditable();
}
	
EWXWEXPORT(void,wxGrid_EnableEditing)(wxGrid* self,bool edit)
{
	self->EnableEditing(edit);
}
	
EWXWEXPORT(void,wxGrid_EnableCellEditControl)(wxGrid* self,bool enable)
{
	self->EnableCellEditControl(enable);
}
	
EWXWEXPORT(void,wxGrid_DisableCellEditControl)(void* _obj)
{
	((wxGrid*)_obj)->DisableCellEditControl();
}
	
EWXWEXPORT(bool,wxGrid_CanEnableCellControl)(wxGrid* self)
{
	return self->CanEnableCellControl();
}
	
EWXWEXPORT(bool,wxGrid_IsCellEditControlEnabled)(wxGrid* self)
{
	return self->IsCellEditControlEnabled();
}
	
EWXWEXPORT(bool,wxGrid_IsCellEditControlShown)(wxGrid* self)
{
	return self->IsCellEditControlShown();
}
	
EWXWEXPORT(bool,wxGrid_IsCurrentCellReadOnly)(wxGrid* self)
{
	return self->IsCurrentCellReadOnly();
}
	
EWXWEXPORT(void,wxGrid_ShowCellEditControl)(void* _obj)
{
	((wxGrid*)_obj)->ShowCellEditControl();
}
	
EWXWEXPORT(void,wxGrid_HideCellEditControl)(void* _obj)
{
	((wxGrid*)_obj)->HideCellEditControl();
}
	
EWXWEXPORT(void,wxGrid_SaveEditControlValue)(void* _obj)
{
	((wxGrid*)_obj)->SaveEditControlValue();
}
	
EWXWEXPORT(void,wxGrid_XYToCell)(void* _obj,int x,int y,int* r,int* c)
{
	wxGridCellCoords cds;
	((wxGrid*)_obj)->XYToCell(x, y, cds);
	*r = cds.GetRow();
	*c = cds.GetCol();
}
	
EWXWEXPORT(int,wxGrid_YToRow)(void* _obj,int y)
{
	return ((wxGrid*)_obj)->YToRow(y);
}
	
EWXWEXPORT(int,wxGrid_XToCol)(void* _obj,int x)
{
	return ((wxGrid*)_obj)->XToCol(x);
}
	
EWXWEXPORT(int,wxGrid_YToEdgeOfRow)(void* _obj,int y)
{
	return ((wxGrid*)_obj)->YToEdgeOfRow(y);
}
	
EWXWEXPORT(int,wxGrid_XToEdgeOfCol)(void* _obj,int x)
{
	return ((wxGrid*)_obj)->XToEdgeOfCol(x);
}
	
EWXWEXPORT(void, wxGrid_CellToRect)(void* _obj, int row, int col, int* x, int* y, int* w, int* h)
{
	wxRect rct = ((wxGrid*)_obj)->CellToRect(row, col);
	*x = rct.x;
	*y = rct.y;
	*w = rct.width;
	*h = rct.height;
}
	
EWXWEXPORT(int,wxGrid_GetGridCursorRow)(void* _obj)
{
	return ((wxGrid*)_obj)->GetGridCursorRow();
}
	
EWXWEXPORT(int,wxGrid_GetGridCursorCol)(void* _obj)
{
	return ((wxGrid*)_obj)->GetGridCursorCol();
}
	
EWXWEXPORT(bool,wxGrid_IsVisible)(wxGrid* self,int row,int col,bool wholeCellVisible)
{
	return self->IsVisible(row, col, wholeCellVisible);
}
	
EWXWEXPORT(void,wxGrid_MakeCellVisible)(void* _obj,int row,int col)
{
	((wxGrid*)_obj)->MakeCellVisible(row, col);
}
	
EWXWEXPORT(void,wxGrid_SetGridCursor)(void* _obj,int row,int col)
{
	((wxGrid*)_obj)->SetGridCursor(row, col);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorUp)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorUp(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorDown)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorDown(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorLeft)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorLeft(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorRight)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorRight(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MovePageDown)(wxGrid* self)
{
	return self->MovePageDown();
}
	
EWXWEXPORT(bool,wxGrid_MovePageUp)(wxGrid* self)
{
	return self->MovePageUp();
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorUpBlock)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorUpBlock(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorDownBlock)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorDownBlock(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorLeftBlock)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorLeftBlock(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorRightBlock)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorRightBlock(expandSelection);
}
	
EWXWEXPORT(int,wxGrid_GetDefaultRowLabelSize)(void* _obj)
{
	return ((wxGrid*)_obj)->GetDefaultRowLabelSize();
}
	
EWXWEXPORT(int,wxGrid_GetRowLabelSize)(void* _obj)
{
	return ((wxGrid*)_obj)->GetRowLabelSize();
}
	
EWXWEXPORT(int,wxGrid_GetDefaultColLabelSize)(void* _obj)
{
	return ((wxGrid*)_obj)->GetDefaultColLabelSize();
}
	
EWXWEXPORT(int,wxGrid_GetColLabelSize)(void* _obj)
{
	return ((wxGrid*)_obj)->GetColLabelSize();
}
	
EWXWEXPORT(void,wxGrid_GetLabelBackgroundColour)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetLabelBackgroundColour();
}
	
EWXWEXPORT(void,wxGrid_GetLabelTextColour)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetLabelTextColour();
}
	
EWXWEXPORT(void,wxGrid_GetLabelFont)(void* _obj,void* font)
{
	*((wxFont*)font) = ((wxGrid*)_obj)->GetLabelFont();
}
	
EWXWEXPORT(void,wxGrid_GetRowLabelAlignment)(void* _obj,int* horiz,int* vert)
{
	((wxGrid*)_obj)->GetRowLabelAlignment(horiz,vert);
}
	
EWXWEXPORT(void,wxGrid_GetColLabelAlignment)(void* _obj,int* horiz,int* vert)
{
	((wxGrid*)_obj)->GetColLabelAlignment(horiz,vert);
}
	
EWXWEXPORT(wxString*,wxGrid_GetRowLabelValue)(void* _obj,int row)
{
	wxString *result = new wxString();
	*result = ((wxGrid*)_obj)->GetRowLabelValue(row);
	return result;
}
	
EWXWEXPORT(wxString*,wxGrid_GetColLabelValue)(void* _obj,int col)
{
	wxString *result = new wxString();
	*result = ((wxGrid*)_obj)->GetColLabelValue(col);
	return result;
}
	
EWXWEXPORT(void,wxGrid_GetGridLineColour)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetGridLineColour();
}
	
EWXWEXPORT(void,wxGrid_GetCellHighlightColour)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetCellHighlightColour();
}
	
EWXWEXPORT(void,wxGrid_SetRowLabelSize)(void* _obj,int width)
{
	((wxGrid*)_obj)->SetRowLabelSize(width);
}
	
EWXWEXPORT(void,wxGrid_SetColLabelSize)(void* _obj,int height)
{
	((wxGrid*)_obj)->SetColLabelSize(height);
}
	
EWXWEXPORT(void,wxGrid_SetLabelBackgroundColour)(void* _obj,void* colour)
{
	((wxGrid*)_obj)->SetLabelBackgroundColour(*((wxColour*)colour));
}
	
EWXWEXPORT(void,wxGrid_SetLabelTextColour)(void* _obj,void* colour)
{
	((wxGrid*)_obj)->SetLabelTextColour(*((wxColour*)colour));
}
	
EWXWEXPORT(void,wxGrid_SetLabelFont)(void* _obj,void* font)
{
	((wxGrid*)_obj)->SetLabelFont(*((wxFont*)font));
}
	
EWXWEXPORT(void,wxGrid_SetRowLabelAlignment)(void* _obj,int horiz,int vert)
{
	((wxGrid*)_obj)->SetRowLabelAlignment(horiz, vert);
}
	
EWXWEXPORT(void,wxGrid_SetColLabelAlignment)(void* _obj,int horiz,int vert)
{
	((wxGrid*)_obj)->SetColLabelAlignment(horiz, vert);
}
	
EWXWEXPORT(void,wxGrid_SetRowLabelValue)(void* _obj,int row,wxString* label)
{
	((wxGrid*)_obj)->SetRowLabelValue(row, *label);
}
	
EWXWEXPORT(void,wxGrid_SetColLabelValue)(void* _obj,int col,wxString* label)
{
	((wxGrid*)_obj)->SetColLabelValue(col, *label);
}
	
EWXWEXPORT(void,wxGrid_SetGridLineColour)(void* _obj,void* col)
{
	((wxGrid*)_obj)->SetGridLineColour(*((wxColour*) col));
}
	
EWXWEXPORT(void,wxGrid_SetCellHighlightColour)(void* _obj,void* col)
{
	((wxGrid*)_obj)->SetCellHighlightColour(*((wxColour*) col));
}
	
EWXWEXPORT(void,wxGrid_EnableDragRowSize)(void* self,bool enable)
{
	((wxGrid*)self)->EnableDragRowSize(enable);
}
	
EWXWEXPORT(void,wxGrid_DisableDragRowSize)(void* self)
{
	((wxGrid*)self)->DisableDragRowSize();
}
	
EWXWEXPORT(bool,wxGrid_CanDragRowSize)(wxGrid* self)
{
	return self->CanDragRowSize();
}
	
EWXWEXPORT(void,wxGrid_EnableDragColSize)(void* self,bool enable)
{
	((wxGrid*)self)->EnableDragColSize(enable);
}
	
EWXWEXPORT(void,wxGrid_DisableDragColSize)(void* self)
{
	((wxGrid*)self)->DisableDragColSize();
}
	
EWXWEXPORT(bool,wxGrid_CanDragColSize)(wxGrid* self)
{
	return self->CanDragColSize();
}
	
EWXWEXPORT(void,wxGrid_EnableDragGridSize)(void* self,bool enable)
{
	((wxGrid*)self)->EnableDragGridSize(enable);
}
	
EWXWEXPORT(void,wxGrid_DisableDragGridSize)(void* self)
{
	((wxGrid*)self)->DisableDragGridSize();
}
	
EWXWEXPORT(bool,wxGrid_CanDragGridSize)(wxGrid* self)
{
	return self->CanDragGridSize();
}
	
EWXWEXPORT(void,wxGrid_SetRowAttr)(void* _obj,int row,void* attr)
{
	((wxGrid*)_obj)->SetRowAttr(row, (wxGridCellAttr*) attr);
}
	
EWXWEXPORT(void,wxGrid_SetColAttr)(void* _obj,int col,void* attr)
{
	((wxGrid*)_obj)->SetColAttr(col, (wxGridCellAttr*) attr);
}
	
EWXWEXPORT(void,wxGrid_SetColFormatBool)(void* self,int col)
{
	((wxGrid*)self)->SetColFormatBool(col);
}
	
EWXWEXPORT(void,wxGrid_SetColFormatNumber)(void* _obj,int col)
{
	((wxGrid*)_obj)->SetColFormatNumber(col);
}
	
EWXWEXPORT(void,wxGrid_SetColFormatFloat)(void* _obj,int col,int width,int precision)
{
	((wxGrid*)_obj)->SetColFormatFloat(col, width, precision);
}
	
EWXWEXPORT(void,wxGrid_SetColFormatCustom)(void* _obj,int col,wxString* typeName)
{
	((wxGrid*)_obj)->SetColFormatCustom(col, *typeName);
}
	
EWXWEXPORT(void,wxGrid_EnableGridLines)(void* self,bool enable)
{
	((wxGrid*)self)->EnableGridLines(enable);
}
	
EWXWEXPORT(bool,wxGrid_GridLinesEnabled)(wxGrid* self)
{
	return self->GridLinesEnabled();
}
	
EWXWEXPORT(int,wxGrid_GetDefaultRowSize)(void* _obj)
{
	return ((wxGrid*)_obj)->GetDefaultRowSize();
}
	
EWXWEXPORT(int,wxGrid_GetRowSize)(void* _obj,int row)
{
	return ((wxGrid*)_obj)->GetRowSize(row);
}
	
EWXWEXPORT(int,wxGrid_GetDefaultColSize)(void* _obj)
{
	return ((wxGrid*)_obj)->GetDefaultColSize();
}
	
EWXWEXPORT(int,wxGrid_GetColSize)(void* _obj,int col)
{
	return ((wxGrid*)_obj)->GetColSize(col);
}
	
EWXWEXPORT(void,wxGrid_GetDefaultCellBackgroundColour)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetDefaultCellBackgroundColour();
}
	
EWXWEXPORT(void,wxGrid_GetCellBackgroundColour)(void* _obj,int row,int col,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetCellBackgroundColour(row, col);
}
	
EWXWEXPORT(void,wxGrid_GetDefaultCellTextColour)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetDefaultCellTextColour();
}
	
EWXWEXPORT(void,wxGrid_GetCellTextColour)(void* _obj,int row,int col,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetCellTextColour(row, col);
}
	
EWXWEXPORT(void,wxGrid_GetDefaultCellFont)(void* _obj,void* font)
{
	*((wxFont*)font) = ((wxGrid*)_obj)->GetDefaultCellFont();
}
	
EWXWEXPORT(void,wxGrid_GetCellFont)(void* _obj,int row,int col,void* font)
{
	*((wxFont*)font) = ((wxGrid*)_obj)->GetCellFont(row, col);
}
	
EWXWEXPORT(void,wxGrid_GetDefaultCellAlignment)(void* _obj,int* horiz,int* vert)
{
	((wxGrid*)_obj)->GetDefaultCellAlignment(horiz,vert);
}
	
EWXWEXPORT(void,wxGrid_GetCellAlignment)(void* _obj,int row,int col,int* horiz,int* vert)
{
	((wxGrid*)_obj)->GetCellAlignment(row, col, horiz,vert);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultRowSize)(void* self,int height,bool resizeExistingRows)
{
	((wxGrid*)self)->SetDefaultRowSize(height, resizeExistingRows);
}
	
EWXWEXPORT(void,wxGrid_SetRowSize)(void* self,int row,int height)
{
	((wxGrid*)self)->SetRowSize(row, height);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultColSize)(void* self,int width,bool resizeExistingCols)
{
	((wxGrid*)self)->SetDefaultColSize(width, resizeExistingCols);
}
	
EWXWEXPORT(void,wxGrid_SetColSize)(void* self,int col,int width)
{
	((wxGrid*)self)->SetColSize(col, width);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeColumn)(void* self,int col,bool setAsMin)
{
	((wxGrid*)self)->AutoSizeColumn(col, setAsMin);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeRow)(void* self,int row,bool setAsMin)
{
	((wxGrid*)self)->AutoSizeRow(row, setAsMin);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeColumns)(void* self,bool setAsMin)
{
	((wxGrid*)self)->AutoSizeColumns(setAsMin);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeRows)(void* self,bool setAsMin)
{
	((wxGrid*)self)->AutoSizeRows(setAsMin);
}
	
EWXWEXPORT(void,wxGrid_AutoSize)(void* self)
{
	((wxGrid*)self)->AutoSize();
}
	
EWXWEXPORT(void,wxGrid_SetColMinimalWidth)(void* _obj,int col,int width)
{
	((wxGrid*)_obj)->SetColMinimalWidth(col, width);
}
	
EWXWEXPORT(void,wxGrid_SetRowMinimalHeight)(void* _obj,int row,int width)
{
	((wxGrid*)_obj)->SetRowMinimalHeight(row, width);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultCellBackgroundColour)(void* _obj,void* colour)
{
	((wxGrid*)_obj)->SetDefaultCellBackgroundColour(*((wxColour*)colour));
}
	
EWXWEXPORT(void,wxGrid_SetCellBackgroundColour)(void* _obj,int row,int col,void* colour)
{
	((wxGrid*)_obj)->SetCellBackgroundColour(row, col, *((wxColour*) colour));
}
	
EWXWEXPORT(void,wxGrid_SetDefaultCellTextColour)(void* _obj,void* colour)
{
	((wxGrid*)_obj)->SetDefaultCellTextColour(*((wxColour*)colour));
}
	
EWXWEXPORT(void,wxGrid_SetCellTextColour)(void* _obj,int row,int col,void* colour)
{
	((wxGrid*)_obj)->SetCellTextColour(row, col, *((wxColour*) colour));
}
	
EWXWEXPORT(void,wxGrid_SetDefaultCellFont)(void* _obj,void* font)
{
	((wxGrid*)_obj)->SetDefaultCellFont(*((wxFont*)font));
}
	
EWXWEXPORT(void,wxGrid_SetCellFont)(void* _obj,int row,int col,void* font)
{
	((wxGrid*)_obj)->SetCellFont(row, col, *((wxFont*)font) );
}
	
EWXWEXPORT(void,wxGrid_SetDefaultCellAlignment)(void* _obj,int horiz,int vert)
{
	((wxGrid*)_obj)->SetDefaultCellAlignment(horiz, vert);
}
	
EWXWEXPORT(void,wxGrid_SetCellAlignment)(void* _obj,int row,int col,int horiz,int vert)
{
	((wxGrid*)_obj)->SetCellAlignment(row, col, horiz, vert);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultRenderer)(void* _obj,void* renderer)
{
	((wxGrid*)_obj)->SetDefaultRenderer((wxGridCellRenderer*) renderer);
}
	
EWXWEXPORT(void,wxGrid_SetCellRenderer)(void* _obj,int row,int col,void* renderer)
{
	((wxGrid*)_obj)->SetCellRenderer(row, col, (wxGridCellRenderer*)renderer);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultRenderer)(void* _obj)
{
	return (void*)((wxGrid*)_obj)->GetDefaultRenderer();
}
	
EWXWEXPORT(void*,wxGrid_GetCellRenderer)(void* _obj,int row,int col)
{
	return (void*)((wxGrid*)_obj)->GetCellRenderer(row, col);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultEditor)(void* _obj,void* editor)
{
	((wxGrid*)_obj)->SetDefaultEditor((wxGridCellEditor*)editor);
}
	
EWXWEXPORT(void,wxGrid_SetCellEditor)(void* _obj,int row,int col,void* editor)
{
	((wxGrid*)_obj)->SetCellEditor(row, col, (wxGridCellEditor*)editor);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultEditor)(void* _obj)
{
	return (void*)((wxGrid*)_obj)->GetDefaultEditor();
}
	
EWXWEXPORT(void*,wxGrid_GetCellEditor)(void* _obj,int row,int col)
{
	return (void*)((wxGrid*)_obj)->GetCellEditor(row, col);
}
	
EWXWEXPORT(wxString*,wxGrid_GetCellValue)(void* _obj,int row,int col)
{
	wxString *result = new wxString();
	*result = ((wxGrid*)_obj)->GetCellValue(row, col);
	return result;
}
	
EWXWEXPORT(void,wxGrid_SetCellValue)(void* _obj,int row,int col,wxString* s)
{
	((wxGrid*)_obj)->SetCellValue(row, col, * s);
}
	
EWXWEXPORT(bool,wxGrid_IsReadOnly)(wxGrid* self,int row,int col)
{
	return self->IsReadOnly(row, col);
}
	
EWXWEXPORT(void,wxGrid_SetReadOnly)(void* self,int row,int col,bool isReadOnly)
{
	((wxGrid*)self)->SetReadOnly(row, col, isReadOnly);
}
	
EWXWEXPORT(void,wxGrid_SelectRow)(void* self,int row,bool addToSelected)
{
	((wxGrid*)self)->SelectRow(row, addToSelected);
}
	
EWXWEXPORT(void,wxGrid_SelectCol)(void* self,int col,bool addToSelected)
{
	((wxGrid*)self)->SelectCol(col, addToSelected);
}
	
EWXWEXPORT(void,wxGrid_SelectBlock)(void* self,int topRow,int leftCol,int bottomRow,int rightCol,bool addToSelected)
{
	((wxGrid*)self)->SelectBlock(topRow, leftCol, bottomRow, rightCol, addToSelected);
}
	
EWXWEXPORT(void,wxGrid_SelectAll)(void* self)
{
	((wxGrid*)self)->SelectAll();
}
	
EWXWEXPORT(bool,wxGrid_IsSelection)(wxGrid* self)
{
	return self->IsSelection();
}
	
EWXWEXPORT(void,wxGrid_ClearSelection)(void* _obj)
{
	((wxGrid*)_obj)->ClearSelection();
}
	
EWXWEXPORT(bool,wxGrid_IsInSelection)(wxGrid* _obj,int row,int col)
{
	return _obj->IsInSelection(row, col );
}
	
EWXWEXPORT(void, wxGrid_BlockToDeviceRect)(void* _obj, int top, int left, int bottom, int right, int* x, int* y, int* w, int* h)
{
	wxRect rct = ((wxGrid*)_obj)->BlockToDeviceRect(wxGridCellCoords(top, left), wxGridCellCoords(bottom, right));
	*x = rct.x;
	*y = rct.y;
	*w = rct.width;
	*h = rct.height;
}
	
EWXWEXPORT(void,wxGrid_GetSelectionBackground)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetSelectionBackground();
}
	
EWXWEXPORT(void,wxGrid_GetSelectionForeground)(void* _obj,void* colour)
{
	*((wxColour*)colour) = ((wxGrid*)_obj)->GetSelectionForeground();
}
	
EWXWEXPORT(void,wxGrid_SetSelectionBackground)(void* _obj,void* c)
{
	((wxGrid*)_obj)->SetSelectionBackground(*((wxColour*) c));
}
	
EWXWEXPORT(void,wxGrid_SetSelectionForeground)(void* _obj,void* c)
{
	((wxGrid*)_obj)->SetSelectionForeground(*((wxColour*) c));
}
	
EWXWEXPORT(void,wxGrid_RegisterDataType)(void* _obj,wxString* typeName,void* renderer,void* editor)
{
	((wxGrid*)_obj)->RegisterDataType(* typeName, (wxGridCellRenderer*) renderer, (wxGridCellEditor*) editor);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultEditorForCell)(void* _obj,int row,int col)
{
	return (void*)((wxGrid*)_obj)->GetDefaultEditorForCell(row, col);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultRendererForCell)(void* _obj,int row,int col)
{
	return (void*)((wxGrid*)_obj)->GetDefaultRendererForCell(row, col);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultEditorForType)(void* _obj,wxString* typeName)
{
	return (void*)((wxGrid*)_obj)->GetDefaultEditorForType(* typeName);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultRendererForType)(void* _obj,wxString* typeName)
{
	return (void*)((wxGrid*)_obj)->GetDefaultRendererForType(* typeName);
}
	
EWXWEXPORT(void,wxGrid_SetMargins)(void* _obj,int extraWidth,int extraHeight)
{
	((wxGrid*)_obj)->SetMargins(extraWidth, extraHeight);
}

EWXWEXPORT(void,wxGrid_GetSelectedCells)(void* _obj,void* _arr)
{
	*((wxGridCellCoordsArray*)_arr) = ((wxGrid*)_obj)->GetSelectedCells();
}
	
EWXWEXPORT(void,wxGrid_GetSelectionBlockTopLeft)(void* _obj,void* _arr)
{
	*((wxGridCellCoordsArray*)_arr) = ((wxGrid*)_obj)->GetSelectionBlockTopLeft();
}
	
EWXWEXPORT(void,wxGrid_GetSelectionBlockBottomRight)(void* _obj,void* _arr)
{
	*((wxGridCellCoordsArray*)_arr) = ((wxGrid*)_obj)->GetSelectionBlockBottomRight();
}
	
EWXWEXPORT(int,wxGrid_GetSelectedRows)(void* _obj,void* _arr)
{
	wxArrayInt arr = ((wxGrid*)_obj)->GetSelectedRows();
	if (_arr)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((int*)_arr)[i] = arr.Item(i);
	}
	return arr.GetCount();
}
	
EWXWEXPORT(int,wxGrid_GetSelectedCols)(void* _obj,void* _arr)
{
	wxArrayInt arr = ((wxGrid*)_obj)->GetSelectedCols();
	if (_arr)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((int*)_arr)[i] = arr.Item(i);
	}
	return arr.GetCount();
}
	


EWXWEXPORT(void*,ELJGridTable_Create)(void* _obj,void* _EifGetNumberRows,void* _EifGetNumberCols,void* _EifGetValue,void* _EifSetValue,void* _EifIsEmptyCell,void* _EifClear,void* _EifInsertRows,void* _EifAppendRows,void* _EifDeleteRows,void* _EifInsertCols,void* _EifAppendCols,void* _EifDeleteCols,void* _EifSetRowLabelValue,void* _EifSetColLabelValue,void* _EifGetRowLabelValue,void* _EifGetColLabelValue)
{
	return (void*)new ELJGridTable (_obj,
	                                _EifGetNumberRows,
	                                _EifGetNumberCols,
	                                _EifGetValue,
	                                _EifSetValue,
	                                _EifIsEmptyCell,
	                                _EifClear,
	                                _EifInsertRows,
	                                _EifAppendRows,
	                                _EifDeleteRows,
	                                _EifInsertCols,
	                                _EifAppendCols,
	                                _EifDeleteCols,
	                                _EifSetRowLabelValue,
	                                _EifSetColLabelValue,
	                                _EifGetRowLabelValue,
	                                _EifGetColLabelValue);
}
	
EWXWEXPORT(void,ELJGridTable_Delete)(void* _obj)
{
	delete (ELJGridTable*)_obj;
}

EWXWEXPORT(void*,ELJGridTable_GetView)(void* _obj)
{
	return (void*)((ELJGridTable*)_obj)->GetView();
}

EWXWEXPORT(void,ELJGridTable_SendTableMessage)(void* _obj,int id,int val1,int val2)
{
	wxGridTableMessage msg((ELJGridTable*)_obj, id, val1, val2);
	((ELJGridTable*)_obj)->GetView()->ProcessTableMessage(msg);
}

EWXWEXPORT(int,wxGridEvent_GetRow)(void* _obj)
{
	return ((wxGridEvent*)_obj)->GetRow();
}
	
EWXWEXPORT(int,wxGridEvent_GetCol)(void* _obj)
{
	return ((wxGridEvent*)_obj)->GetCol();
}
	
EWXWEXPORT(void,wxGridEvent_GetPosition)(void* _obj,void* _x,void* _y)
{
	wxPoint pt = ((wxGridEvent*)_obj)->GetPosition();
	*((int*)_x) = pt.x;
	*((int*)_y) = pt.y;
}
	
EWXWEXPORT(bool,wxGridEvent_Selecting)(wxGridEvent* self)
{
	return self->Selecting();
}
	
EWXWEXPORT(bool,wxGridEvent_ControlDown)(wxGridEvent* self)
{
	return self->ControlDown();
}
	
EWXWEXPORT(bool,wxGridEvent_MetaDown)(wxGridEvent* self)
{
	return self->MetaDown();
}
	
EWXWEXPORT(bool,wxGridEvent_ShiftDown)(wxGridEvent* self)
{
	return self->ShiftDown();
}
	
EWXWEXPORT(bool,wxGridEvent_AltDown)(wxGridEvent* self)
{
	return self->AltDown();
}
	

EWXWEXPORT(int,wxGridSizeEvent_GetRowOrCol)(void* _obj)
{
	return ((wxGridSizeEvent*)_obj)->GetRowOrCol();
}
	
EWXWEXPORT(void,wxGridSizeEvent_GetPosition)(void* _obj,void* _x, void* _y)
{
	wxPoint pt = ((wxGridSizeEvent*)_obj)->GetPosition();
	*((int*)_x) = pt.x;
	*((int*)_y) = pt.y;
}
	
EWXWEXPORT(bool,wxGridSizeEvent_ControlDown)(wxGridSizeEvent* self)
{
	return self->ControlDown();
}
	
EWXWEXPORT(bool,wxGridSizeEvent_MetaDown)(wxGridSizeEvent* self)
{
	return self->MetaDown();
}
	
EWXWEXPORT(bool,wxGridSizeEvent_ShiftDown)(wxGridSizeEvent* self)
{
	return self->ShiftDown();
}
	
EWXWEXPORT(bool,wxGridSizeEvent_AltDown)(wxGridSizeEvent* self)
{
	return self->AltDown();
}
	

EWXWEXPORT(void,wxGridRangeSelectEvent_GetTopLeftCoords)(void* _obj,int* _c,int* _r)
{
	wxGridCellCoords crd = ((wxGridRangeSelectEvent*)_obj)->GetTopLeftCoords();
	*_c = crd.GetRow();
	*_r = crd.GetCol();
}
	
EWXWEXPORT(void,wxGridRangeSelectEvent_GetBottomRightCoords)(void* _obj,int* _c,int* _r)
{
	wxGridCellCoords crd = ((wxGridRangeSelectEvent*)_obj)->GetBottomRightCoords();
	*_c = crd.GetRow();
	*_r = crd.GetCol();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_GetTopRow)(void* _obj)
{
	return ((wxGridRangeSelectEvent*)_obj)->GetTopRow();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_GetBottomRow)(void* _obj)
{
	return ((wxGridRangeSelectEvent*)_obj)->GetBottomRow();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_GetLeftCol)(void* _obj)
{
	return ((wxGridRangeSelectEvent*)_obj)->GetLeftCol();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_GetRightCol)(void* _obj)
{
	return ((wxGridRangeSelectEvent*)_obj)->GetRightCol();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_Selecting)(wxGridRangeSelectEvent* self)
{
	return self->Selecting();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_ControlDown)(wxGridRangeSelectEvent* self)
{
	return self->ControlDown();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_MetaDown)(wxGridRangeSelectEvent* self)
{
	return self->MetaDown();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_ShiftDown)(wxGridRangeSelectEvent* self)
{
	return self->ShiftDown();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_AltDown)(wxGridRangeSelectEvent* self)
{
	return self->AltDown();
}
	

EWXWEXPORT(int,wxGridEditorCreatedEvent_GetRow)(void* _obj)
{
	return ((wxGridEditorCreatedEvent*)_obj)->GetRow();
}
	
EWXWEXPORT(int,wxGridEditorCreatedEvent_GetCol)(void* _obj)
{
	return ((wxGridEditorCreatedEvent*)_obj)->GetCol();
}
	
EWXWEXPORT(void*,wxGridEditorCreatedEvent_GetControl)(void* _obj)
{
	return (void*)((wxGridEditorCreatedEvent*)_obj)->GetControl();
}
	
EWXWEXPORT(void,wxGridEditorCreatedEvent_SetRow)(void* _obj,int row)
{
	((wxGridEditorCreatedEvent*)_obj)->SetRow(row);
}
	
EWXWEXPORT(void,wxGridEditorCreatedEvent_SetCol)(void* _obj,int col)
{
	((wxGridEditorCreatedEvent*)_obj)->SetCol(col);
}
	
EWXWEXPORT(void,wxGridEditorCreatedEvent_SetControl)(void* _obj,void* ctrl)
{
	((wxGridEditorCreatedEvent*)_obj)->SetControl((wxControl*)ctrl);
}
	

} 
