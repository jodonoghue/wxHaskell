#include "wrapper.h"
#include "eljgrid.h"

extern "C"
{

EWXWEXPORT(void*,wxGridCellCoordsArray_Create)()
{
	return (void*) new wxGridCellCoordsArray();
}

EWXWEXPORT(void,wxGridCellCoordsArray_Delete)(void* _obj)
{
	delete (wxGridCellCoordsArray*)_obj;
}

EWXWEXPORT(int,wxGridCellCoordsArray_GetCount)(void* _obj)
{
	return ((wxGridCellCoordsArray*)_obj)->GetCount();
}

EWXWEXPORT(void,wxGridCellCoordsArray_Item)(void* _obj,int _idx,int* _c,int* _r)
{
	*_c = ((wxGridCellCoordsArray*)_obj)->Item(_idx).GetCol();
	*_r = ((wxGridCellCoordsArray*)_obj)->Item(_idx).GetRow();
}


EWXWEXPORT(int,wxGridCellEditor_IsCreated)(wxGridCellEditor* _obj)
{
	return (int)_obj->IsCreated();
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
	
EWXWEXPORT(void,wxGridCellEditor_Show)(void* _obj,int show,void* attr)
{
	((wxGridCellEditor*)_obj)->Show(show != 0, (wxGridCellAttr*) attr);
}
	
EWXWEXPORT(void,wxGridCellEditor_PaintBackground)(void* _obj,int x,int y,int w,int h,void* attr)
{
	((wxGridCellEditor*)_obj)->PaintBackground(wxRect(x, y, w, h), (wxGridCellAttr*)attr);
}
	
EWXWEXPORT(void,wxGridCellEditor_BeginEdit)(void* _obj,int row,int col,void* grid)
{
	((wxGridCellEditor*)_obj)->BeginEdit(row, col, (wxGrid*)grid);
}
	
EWXWEXPORT(int,wxGridCellEditor_EndEdit)(wxGridCellEditor* _obj,int row,int col,wxGrid* grid)
{
	return (int)_obj->EndEdit(row, col,  grid);
}
	
EWXWEXPORT(void,wxGridCellEditor_Reset)(void* _obj)
{
	((wxGridCellEditor*)_obj)->Reset();
}
	
EWXWEXPORT(int,wxGridCellEditor_IsAcceptedKey)(wxGridCellEditor* _obj,wxKeyEvent* event)
{
	return (int)_obj->IsAcceptedKey(*event);
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

EWXWEXPORT(void*,wxGridCellChoiceEditor_Ctor)(int count,void* choices,int allowOthers)
{
	wxString items[256];

	for (int i = 0; i < count; i++)
		items[i] = ((wxChar**)choices)[i];

	return (void*) new wxGridCellChoiceEditor (count, items, allowOthers != 0);
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
	
EWXWEXPORT(void,wxGridCellAttr_SetReadOnly)(void* _obj,int isReadOnly)
{
	((wxGridCellAttr*)_obj)->SetReadOnly(isReadOnly != 0);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetRenderer)(void* _obj,void* renderer)
{
	((wxGridCellAttr*)_obj)->SetRenderer((wxGridCellRenderer*)renderer);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetEditor)(void* _obj,void* editor)
{
	((wxGridCellAttr*)_obj)->SetEditor((wxGridCellEditor*) editor);
}
	
EWXWEXPORT(int,wxGridCellAttr_HasTextColour)(wxGridCellAttr* _obj)
{
	return (int)_obj->HasTextColour();
}
	
EWXWEXPORT(int,wxGridCellAttr_HasBackgroundColour)(wxGridCellAttr* _obj)
{
	return (int)_obj->HasBackgroundColour();
}
	
EWXWEXPORT(int,wxGridCellAttr_HasFont)(wxGridCellAttr* _obj)
{
	return (int)_obj->HasFont();
}
	
EWXWEXPORT(int,wxGridCellAttr_HasAlignment)(wxGridCellAttr* _obj)
{
	return (int)_obj->HasAlignment();
}
	
EWXWEXPORT(int,wxGridCellAttr_HasRenderer)(wxGridCellAttr* _obj)
{
	return (int)_obj->HasRenderer();
}
	
EWXWEXPORT(int,wxGridCellAttr_HasEditor)(wxGridCellAttr* _obj)
{
	return (int)_obj->HasEditor();
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
	
EWXWEXPORT(int,wxGridCellAttr_IsReadOnly)(wxGridCellAttr* _obj)
{
	return (int)_obj->IsReadOnly();
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
	
EWXWEXPORT(int,wxGrid_ProcessTableMessage)(wxGrid* _obj,wxGridTableMessage* evt)
{
	return (int)_obj->ProcessTableMessage(*evt);
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
	
EWXWEXPORT(int,wxGrid_SetTable)(wxGrid* _obj,wxGridTableBase* table,int takeOwnership,int selmode)
{
	return (int)_obj->SetTable(table, takeOwnership != 0, (wxGrid::wxGridSelectionModes) selmode);
}
	
EWXWEXPORT(void,wxGrid_ClearGrid)(void* _obj)
{
	((wxGrid*)_obj)->ClearGrid();
}
	
EWXWEXPORT(int,wxGrid_InsertRows)(wxGrid* _obj,int pos,int numRows,int updateLabels)
{
	return (int)_obj->InsertRows(pos, numRows, updateLabels != 0);
}
	
EWXWEXPORT(int,wxGrid_AppendRows)(wxGrid* _obj,int numRows,int updateLabels)
{
	return (int)_obj->AppendRows(numRows, updateLabels != 0);
}
	
EWXWEXPORT(int,wxGrid_DeleteRows)(wxGrid* _obj,int pos,int numRows,int updateLabels)
{
	return (int)_obj->DeleteRows(pos, numRows, updateLabels != 0);
}
	
EWXWEXPORT(int,wxGrid_InsertCols)(wxGrid* _obj,int pos,int numCols,int updateLabels)
{
	return (int)_obj->InsertCols(pos, numCols, updateLabels);
}
	
EWXWEXPORT(int,wxGrid_AppendCols)(wxGrid* _obj,int numCols,int updateLabels)
{
	return (int)_obj->AppendCols( numCols, updateLabels != 0);
}
	
EWXWEXPORT(int,wxGrid_DeleteCols)(wxGrid* _obj,int pos,int numCols,int updateLabels)
{
	return (int)_obj->DeleteCols(pos, numCols, updateLabels != 0);
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
	
EWXWEXPORT(int,wxGrid_IsEditable)(wxGrid* _obj)
{
	return (int)_obj->IsEditable();
}
	
EWXWEXPORT(void,wxGrid_EnableEditing)(void* _obj,int edit)
{
	((wxGrid*)_obj)->EnableEditing(edit != 0);
}
	
EWXWEXPORT(void,wxGrid_EnableCellEditControl)(void* _obj,int enable)
{
	((wxGrid*)_obj)->EnableCellEditControl(enable != 0);
}
	
EWXWEXPORT(void,wxGrid_DisableCellEditControl)(void* _obj)
{
	((wxGrid*)_obj)->DisableCellEditControl();
}
	
EWXWEXPORT(int,wxGrid_CanEnableCellControl)(wxGrid* _obj)
{
	return (int)_obj->CanEnableCellControl();
}
	
EWXWEXPORT(int,wxGrid_IsCellEditControlEnabled)(wxGrid* _obj)
{
	return (int)_obj->IsCellEditControlEnabled();
}
	
EWXWEXPORT(int,wxGrid_IsCellEditControlShown)(wxGrid* _obj)
{
	return (int)_obj->IsCellEditControlShown();
}
	
EWXWEXPORT(int,wxGrid_IsCurrentCellReadOnly)(wxGrid* _obj)
{
	return (int)_obj->IsCurrentCellReadOnly();
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
	
EWXWEXPORT(int,wxGrid_IsVisible)(wxGrid* _obj,int row,int col,int wholeCellVisible)
{
	return (int)_obj->IsVisible(row, col, wholeCellVisible != 0);
}
	
EWXWEXPORT(void,wxGrid_MakeCellVisible)(void* _obj,int row,int col)
{
	((wxGrid*)_obj)->MakeCellVisible(row, col);
}
	
EWXWEXPORT(void,wxGrid_SetGridCursor)(void* _obj,int row,int col)
{
	((wxGrid*)_obj)->SetGridCursor(row, col);
}
	
EWXWEXPORT(int,wxGrid_MoveCursorUp)(wxGrid* _obj,int expandSelection)
{
	return (int)_obj->MoveCursorUp(expandSelection != 0);
}
	
EWXWEXPORT(int,wxGrid_MoveCursorDown)(wxGrid* _obj,int expandSelection)
{
	return (int)_obj->MoveCursorDown(expandSelection != 0);
}
	
EWXWEXPORT(int,wxGrid_MoveCursorLeft)(wxGrid* _obj,int expandSelection)
{
	return (int)_obj->MoveCursorLeft(expandSelection != 0);
}
	
EWXWEXPORT(int,wxGrid_MoveCursorRight)(wxGrid* _obj,int expandSelection)
{
	return (int)_obj->MoveCursorRight(expandSelection != 0);
}
	
EWXWEXPORT(int,wxGrid_MovePageDown)(wxGrid* _obj)
{
	return (int)_obj->MovePageDown();
}
	
EWXWEXPORT(int,wxGrid_MovePageUp)(wxGrid* _obj)
{
	return (int)_obj->MovePageUp();
}
	
EWXWEXPORT(int,wxGrid_MoveCursorUpBlock)(wxGrid* _obj,int expandSelection)
{
	return (int)_obj->MoveCursorUpBlock(expandSelection != 0);
}
	
EWXWEXPORT(int,wxGrid_MoveCursorDownBlock)(wxGrid* _obj,int expandSelection)
{
	return (int)_obj->MoveCursorDownBlock(expandSelection != 0);
}
	
EWXWEXPORT(int,wxGrid_MoveCursorLeftBlock)(wxGrid* _obj,int expandSelection)
{
	return (int)_obj->MoveCursorLeftBlock(expandSelection != 0);
}
	
EWXWEXPORT(int,wxGrid_MoveCursorRightBlock)(wxGrid* _obj,int expandSelection)
{
	return (int)_obj->MoveCursorRightBlock(expandSelection != 0);
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
	
EWXWEXPORT(void,wxGrid_EnableDragRowSize)(void* _obj,int enable)
{
	((wxGrid*)_obj)->EnableDragRowSize(enable != 0);
}
	
EWXWEXPORT(void,wxGrid_DisableDragRowSize)(void* _obj)
{
	((wxGrid*)_obj)->DisableDragRowSize();
}
	
EWXWEXPORT(int,wxGrid_CanDragRowSize)(wxGrid* _obj)
{
	return (int)_obj->CanDragRowSize();
}
	
EWXWEXPORT(void,wxGrid_EnableDragColSize)(void* _obj,int enable)
{
	((wxGrid*)_obj)->EnableDragColSize(enable != 0);
}
	
EWXWEXPORT(void,wxGrid_DisableDragColSize)(void* _obj)
{
	((wxGrid*)_obj)->DisableDragColSize();
}
	
EWXWEXPORT(int,wxGrid_CanDragColSize)(wxGrid* _obj)
{
	return (int)_obj->CanDragColSize();
}
	
EWXWEXPORT(void,wxGrid_EnableDragGridSize)(void* _obj,int enable)
{
	((wxGrid*)_obj)->EnableDragGridSize(enable != 0);
}
	
EWXWEXPORT(void,wxGrid_DisableDragGridSize)(void* _obj)
{
	((wxGrid*)_obj)->DisableDragGridSize();
}
	
EWXWEXPORT(int,wxGrid_CanDragGridSize)(wxGrid* _obj)
{
	return (int)_obj->CanDragGridSize();
}
	
EWXWEXPORT(void,wxGrid_SetRowAttr)(void* _obj,int row,void* attr)
{
	((wxGrid*)_obj)->SetRowAttr(row, (wxGridCellAttr*) attr);
}
	
EWXWEXPORT(void,wxGrid_SetColAttr)(void* _obj,int col,void* attr)
{
	((wxGrid*)_obj)->SetColAttr(col, (wxGridCellAttr*) attr);
}
	
EWXWEXPORT(void,wxGrid_SetColFormatBool)(void* _obj,int col)
{
	((wxGrid*)_obj)->SetColFormatBool(col != 0);
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
	
EWXWEXPORT(void,wxGrid_EnableGridLines)(void* _obj,int enable)
{
	((wxGrid*)_obj)->EnableGridLines(enable != 0);
}
	
EWXWEXPORT(int,wxGrid_GridLinesEnabled)(wxGrid* _obj)
{
	return (int)_obj->GridLinesEnabled();
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
	
EWXWEXPORT(void,wxGrid_SetDefaultRowSize)(void* _obj,int height,int resizeExistingRows)
{
	((wxGrid*)_obj)->SetDefaultRowSize(height, resizeExistingRows != 0);
}
	
EWXWEXPORT(void,wxGrid_SetRowSize)(void* _obj,int row,int height)
{
	((wxGrid*)_obj)->SetRowSize(row, height);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultColSize)(void* _obj,int width,int resizeExistingCols)
{
	((wxGrid*)_obj)->SetDefaultColSize(width, resizeExistingCols != 0);
}
	
EWXWEXPORT(void,wxGrid_SetColSize)(void* _obj,int col,int width)
{
	((wxGrid*)_obj)->SetColSize(col, width);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeColumn)(void* _obj,int col,int setAsMin)
{
	((wxGrid*)_obj)->AutoSizeColumn(col, setAsMin != 0);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeRow)(void* _obj,int row,int setAsMin)
{
	((wxGrid*)_obj)->AutoSizeRow(row, setAsMin != 0);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeColumns)(void* _obj,int setAsMin)
{
	((wxGrid*)_obj)->AutoSizeColumns(setAsMin != 0);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeRows)(void* _obj,int setAsMin)
{
	((wxGrid*)_obj)->AutoSizeRows(setAsMin != 0);
}
	
EWXWEXPORT(void,wxGrid_AutoSize)(void* _obj)
{
	((wxGrid*)_obj)->AutoSize();
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
	
EWXWEXPORT(int,wxGrid_IsReadOnly)(wxGrid* _obj,int row,int col)
{
	return (int)_obj->IsReadOnly(row, col);
}
	
EWXWEXPORT(void,wxGrid_SetReadOnly)(void* _obj,int row,int col,int isReadOnly)
{
	((wxGrid*)_obj)->SetReadOnly(row, col, isReadOnly != 0);
}
	
EWXWEXPORT(void,wxGrid_SelectRow)(void* _obj,int row,int addToSelected)
{
	((wxGrid*)_obj)->SelectRow(row, addToSelected != 0);
}
	
EWXWEXPORT(void,wxGrid_SelectCol)(void* _obj,int col,int addToSelected)
{
	((wxGrid*)_obj)->SelectCol(col, addToSelected != 0);
}
	
EWXWEXPORT(void,wxGrid_SelectBlock)(void* _obj,int topRow,int leftCol,int bottomRow,int rightCol,int addToSelected)
{
	((wxGrid*)_obj)->SelectBlock(topRow, leftCol, bottomRow, rightCol, addToSelected != 0);
}
	
EWXWEXPORT(void,wxGrid_SelectAll)(void* _obj)
{
	((wxGrid*)_obj)->SelectAll();
}
	
EWXWEXPORT(int,wxGrid_IsSelection)(wxGrid* _obj)
{
	return (int)_obj->IsSelection();
}
	
EWXWEXPORT(void,wxGrid_ClearSelection)(void* _obj)
{
	((wxGrid*)_obj)->ClearSelection();
}
	
EWXWEXPORT(int,wxGrid_IsInSelection)(wxGrid* _obj,int row,int col)
{
	return (int)_obj->IsInSelection(row, col );
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
	
EWXWEXPORT(int,wxGridEvent_Selecting)(wxGridEvent* _obj)
{
	return (int)_obj->Selecting();
}
	
EWXWEXPORT(int,wxGridEvent_ControlDown)(wxGridEvent* _obj)
{
	return (int)_obj->ControlDown();
}
	
EWXWEXPORT(int,wxGridEvent_MetaDown)(wxGridEvent* _obj)
{
	return (int)_obj->MetaDown();
}
	
EWXWEXPORT(int,wxGridEvent_ShiftDown)(wxGridEvent* _obj)
{
	return (int)_obj->ShiftDown();
}
	
EWXWEXPORT(int,wxGridEvent_AltDown)(wxGridEvent* _obj)
{
	return (int)_obj->AltDown();
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
	
EWXWEXPORT(int,wxGridSizeEvent_ControlDown)(wxGridSizeEvent* _obj)
{
	return (int)_obj->ControlDown();
}
	
EWXWEXPORT(int,wxGridSizeEvent_MetaDown)(wxGridSizeEvent* _obj)
{
	return (int) _obj->MetaDown();
}
	
EWXWEXPORT(int,wxGridSizeEvent_ShiftDown)(wxGridSizeEvent* _obj)
{
	return (int)_obj->ShiftDown();
}
	
EWXWEXPORT(int,wxGridSizeEvent_AltDown)(wxGridSizeEvent* _obj)
{
	return (int)_obj->AltDown();
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
	
EWXWEXPORT(int,wxGridRangeSelectEvent_Selecting)(wxGridRangeSelectEvent* _obj)
{
	return (int)_obj->Selecting();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_ControlDown)(wxGridRangeSelectEvent* _obj)
{
	return (int)_obj->ControlDown();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_MetaDown)(wxGridRangeSelectEvent* _obj)
{
	return (int)_obj->MetaDown();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_ShiftDown)(wxGridRangeSelectEvent* _obj)
{
	return (int)_obj->ShiftDown();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_AltDown)(wxGridRangeSelectEvent* _obj)
{
	return (int)_obj->AltDown();
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
