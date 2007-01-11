EWXWEXPORT(void, wxStyledTextCtrl_AddText)(void* _obj, wxChar* text)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AddText(text);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AddStyledText)(void* _obj, void* data)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AddStyledText(*(wxMemoryBuffer*) data);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_InsertText)(void* _obj, int pos, wxChar* text)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->InsertText(pos, text);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ClearAll)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ClearAll();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ClearDocumentStyle)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ClearDocumentStyle();
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetLength)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLength();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetCharAt)(void* _obj, int pos)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetCharAt(pos);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetCurrentPos)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetCurrentPos();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetAnchor)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetAnchor();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetStyleAt)(void* _obj, int pos)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetStyleAt(pos);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_Redo)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->Redo();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetUndoCollection)(void* _obj, bool collectUndo)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetUndoCollection(collectUndo);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SelectAll)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SelectAll();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetSavePoint)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetSavePoint();
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_CanRedo)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->CanRedo();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_MarkerLineFromHandle)(void* _obj, int handle)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->MarkerLineFromHandle(handle);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_MarkerDeleteHandle)(void* _obj, int handle)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->MarkerDeleteHandle(handle);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetUndoCollection)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetUndoCollection();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetViewWhiteSpace)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetViewWhiteSpace();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetViewWhiteSpace)(void* _obj, int viewWS)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetViewWhiteSpace(viewWS);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_PositionFromPoint)(void* _obj, int pt_x, int pt_y)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->PositionFromPoint(wxPoint(pt_x,pt_y));
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_PositionFromPointClose)(void* _obj, int x, int y)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->PositionFromPointClose(x, y);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_GotoLine)(void* _obj, int line)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->GotoLine(line);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_GotoPos)(void* _obj, int pos)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->GotoPos(pos);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetAnchor)(void* _obj, int posAnchor)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetAnchor(posAnchor);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetEndStyled)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetEndStyled();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ConvertEOLs)(void* _obj, int eolMode)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ConvertEOLs(eolMode);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetEOLMode)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetEOLMode();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetEOLMode)(void* _obj, int eolMode)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetEOLMode(eolMode);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StartStyling)(void* _obj, int pos, int mask)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StartStyling(pos, mask);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetStyling)(void* _obj, int length, int style)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetStyling(length, style);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetBufferedDraw)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetBufferedDraw();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetBufferedDraw)(void* _obj, bool buffered)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetBufferedDraw(buffered);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetTabWidth)(void* _obj, int tabWidth)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetTabWidth(tabWidth);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetTabWidth)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetTabWidth();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetCodePage)(void* _obj, int codePage)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetCodePage(codePage);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_MarkerDefine)(void* _obj, int markerNumber, int markerSymbol, int foreground_r, int foreground_g, int foreground_b, int background_r, int background_g, int background_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->MarkerDefine(markerNumber, markerSymbol, wxColour(foreground_r,foreground_g,foreground_b), wxColour(background_r,background_g,background_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_MarkerSetForeground)(void* _obj, int markerNumber, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->MarkerSetForeground(markerNumber, wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_MarkerSetBackground)(void* _obj, int markerNumber, int back_r, int back_g, int back_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->MarkerSetBackground(markerNumber, wxColour(back_r,back_g,back_b));
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_MarkerAdd)(void* _obj, int line, int markerNumber)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->MarkerAdd(line, markerNumber);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_MarkerDelete)(void* _obj, int line, int markerNumber)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->MarkerDelete(line, markerNumber);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_MarkerDeleteAll)(void* _obj, int markerNumber)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->MarkerDeleteAll(markerNumber);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_MarkerGet)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->MarkerGet(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_MarkerNext)(void* _obj, int lineStart, int markerMask)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->MarkerNext(lineStart, markerMask);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_MarkerPrevious)(void* _obj, int lineStart, int markerMask)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->MarkerPrevious(lineStart, markerMask);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_MarkerDefineBitmap)(void* _obj, int markerNumber, void* bmp)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->MarkerDefineBitmap(markerNumber, *(wxBitmap*) bmp);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetMarginType)(void* _obj, int margin, int marginType)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetMarginType(margin, marginType);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetMarginType)(void* _obj, int margin)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetMarginType(margin);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetMarginWidth)(void* _obj, int margin, int pixelWidth)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetMarginWidth(margin, pixelWidth);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetMarginWidth)(void* _obj, int margin)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetMarginWidth(margin);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetMarginMask)(void* _obj, int margin, int mask)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetMarginMask(margin, mask);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetMarginMask)(void* _obj, int margin)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetMarginMask(margin);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetMarginSensitive)(void* _obj, int margin, bool sensitive)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetMarginSensitive(margin, sensitive);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetMarginSensitive)(void* _obj, int margin)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetMarginSensitive(margin);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleClearAll)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleClearAll();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetForeground)(void* _obj, int style, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetForeground(style, wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetBackground)(void* _obj, int style, int back_r, int back_g, int back_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetBackground(style, wxColour(back_r,back_g,back_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetBold)(void* _obj, int style, bool bold)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetBold(style, bold);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetItalic)(void* _obj, int style, bool italic)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetItalic(style, italic);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetSize)(void* _obj, int style, int sizePoints)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetSize(style, sizePoints);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetFaceName)(void* _obj, int style, wxChar* fontName)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetFaceName(style, fontName);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetEOLFilled)(void* _obj, int style, bool filled)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetEOLFilled(style, filled);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleResetDefault)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleResetDefault();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetUnderline)(void* _obj, int style, bool underline)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetUnderline(style, underline);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetCase)(void* _obj, int style, int caseForce)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetCase(style, caseForce);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetCharacterSet)(void* _obj, int style, int characterSet)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetCharacterSet(style, characterSet);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetHotSpot)(void* _obj, int style, bool hotspot)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetHotSpot(style, hotspot);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetSelForeground)(void* _obj, bool useSetting, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetSelForeground(useSetting, wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetSelBackground)(void* _obj, bool useSetting, int back_r, int back_g, int back_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetSelBackground(useSetting, wxColour(back_r,back_g,back_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetCaretForeground)(void* _obj, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetCaretForeground(wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CmdKeyAssign)(void* _obj, int key, int modifiers, int cmd)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CmdKeyAssign(key, modifiers, cmd);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CmdKeyClear)(void* _obj, int key, int modifiers)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CmdKeyClear(key, modifiers);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CmdKeyClearAll)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CmdKeyClearAll();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetStyleBytes)(void* _obj, int length, void* styleBytes)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetStyleBytes(length, *(char**) styleBytes);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetVisible)(void* _obj, int style, bool visible)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetVisible(style, visible);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetCaretPeriod)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetCaretPeriod();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetCaretPeriod)(void* _obj, int periodMilliseconds)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetCaretPeriod(periodMilliseconds);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetWordChars)(void* _obj, wxChar* characters)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetWordChars(characters);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_BeginUndoAction)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->BeginUndoAction();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_EndUndoAction)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->EndUndoAction();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_IndicatorSetStyle)(void* _obj, int indic, int style)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->IndicatorSetStyle(indic, style);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_IndicatorGetStyle)(void* _obj, int indic)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->IndicatorGetStyle(indic);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_IndicatorSetForeground)(void* _obj, int indic, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->IndicatorSetForeground(indic, wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetWhitespaceForeground)(void* _obj, bool useSetting, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetWhitespaceForeground(useSetting, wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetWhitespaceBackground)(void* _obj, bool useSetting, int back_r, int back_g, int back_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetWhitespaceBackground(useSetting, wxColour(back_r,back_g,back_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetStyleBits)(void* _obj, int bits)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetStyleBits(bits);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetStyleBits)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetStyleBits();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetLineState)(void* _obj, int line, int state)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetLineState(line, state);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetLineState)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLineState(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetMaxLineState)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetMaxLineState();
#else
  return NULL;
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetCaretLineVisible)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetCaretLineVisible();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetCaretLineVisible)(void* _obj, bool show)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetCaretLineVisible(show);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetCaretLineBack)(void* _obj, int back_r, int back_g, int back_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetCaretLineBack(wxColour(back_r,back_g,back_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetChangeable)(void* _obj, int style, bool changeable)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetChangeable(style, changeable);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompShow)(void* _obj, int lenEntered, wxChar* itemList)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompShow(lenEntered, itemList);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompCancel)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompCancel();
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_AutoCompActive)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->AutoCompActive();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_AutoCompPosStart)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->AutoCompPosStart();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompComplete)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompComplete();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompStops)(void* _obj, wxChar* characterSet)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompStops(characterSet);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompSetSeparator)(void* _obj, int separatorCharacter)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompSetSeparator(separatorCharacter);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_AutoCompGetSeparator)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->AutoCompGetSeparator();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompSelect)(void* _obj, wxChar* text)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompSelect(text);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompSetCancelAtStart)(void* _obj, bool cancel)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompSetCancelAtStart(cancel);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_AutoCompGetCancelAtStart)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->AutoCompGetCancelAtStart();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompSetFillUps)(void* _obj, wxChar* characterSet)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompSetFillUps(characterSet);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompSetChooseSingle)(void* _obj, bool chooseSingle)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompSetChooseSingle(chooseSingle);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_AutoCompGetChooseSingle)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->AutoCompGetChooseSingle();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompSetIgnoreCase)(void* _obj, bool ignoreCase)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompSetIgnoreCase(ignoreCase);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_AutoCompGetIgnoreCase)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->AutoCompGetIgnoreCase();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_UserListShow)(void* _obj, int listType, wxChar* itemList)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->UserListShow(listType, itemList);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompSetAutoHide)(void* _obj, bool autoHide)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompSetAutoHide(autoHide);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_AutoCompGetAutoHide)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->AutoCompGetAutoHide();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompSetDropRestOfWord)(void* _obj, bool dropRestOfWord)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompSetDropRestOfWord(dropRestOfWord);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_AutoCompGetDropRestOfWord)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->AutoCompGetDropRestOfWord();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_RegisterImage)(void* _obj, int type, void* bmp)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->RegisterImage(type, *(wxBitmap*) bmp);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ClearRegisteredImages)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ClearRegisteredImages();
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_AutoCompGetTypeSeparator)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->AutoCompGetTypeSeparator();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AutoCompSetTypeSeparator)(void* _obj, int separatorCharacter)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AutoCompSetTypeSeparator(separatorCharacter);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetIndent)(void* _obj, int indentSize)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetIndent(indentSize);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetIndent)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetIndent();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetUseTabs)(void* _obj, bool useTabs)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetUseTabs(useTabs);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetUseTabs)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetUseTabs();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetLineIndentation)(void* _obj, int line, int indentSize)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetLineIndentation(line, indentSize);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetLineIndentation)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLineIndentation(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetLineIndentPosition)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLineIndentPosition(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetColumn)(void* _obj, int pos)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetColumn(pos);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetUseHorizontalScrollBar)(void* _obj, bool show)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetUseHorizontalScrollBar(show);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetUseHorizontalScrollBar)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetUseHorizontalScrollBar();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetIndentationGuides)(void* _obj, bool show)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetIndentationGuides(show);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetIndentationGuides)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetIndentationGuides();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetHighlightGuide)(void* _obj, int column)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetHighlightGuide(column);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetHighlightGuide)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetHighlightGuide();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetLineEndPosition)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLineEndPosition(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetCodePage)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetCodePage();
#else
  return NULL;
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetReadOnly)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetReadOnly();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetCurrentPos)(void* _obj, int pos)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetCurrentPos(pos);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetSelectionStart)(void* _obj, int pos)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetSelectionStart(pos);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetSelectionStart)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetSelectionStart();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetSelectionEnd)(void* _obj, int pos)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetSelectionEnd(pos);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetSelectionEnd)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetSelectionEnd();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetPrintMagnification)(void* _obj, int magnification)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetPrintMagnification(magnification);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetPrintMagnification)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetPrintMagnification();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetPrintColourMode)(void* _obj, int mode)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetPrintColourMode(mode);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetPrintColourMode)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetPrintColourMode();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_FindText)(void* _obj, int minPos, int maxPos, wxChar* text, int flags)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->FindText(minPos, maxPos, text, flags);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_FormatRange)(void* _obj, bool doDraw, int startPos, int endPos, void* draw, void* target, void* renderRect, void* pageRect)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->FormatRange(doDraw, startPos, endPos, *(wxDC**) draw, *(wxDC**) target, *(wxRect*) renderRect, *(wxRect*) pageRect);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetFirstVisibleLine)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetFirstVisibleLine();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetLineCount)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLineCount();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetMarginLeft)(void* _obj, int pixelWidth)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetMarginLeft(pixelWidth);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetMarginLeft)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetMarginLeft();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetMarginRight)(void* _obj, int pixelWidth)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetMarginRight(pixelWidth);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetMarginRight)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetMarginRight();
#else
  return NULL;
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetModify)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetModify();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetSelection)(void* _obj, int start, int end)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetSelection(start, end);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_HideSelection)(void* _obj, bool normal)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->HideSelection(normal);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_LineFromPosition)(void* _obj, int pos)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->LineFromPosition(pos);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_PositionFromLine)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->PositionFromLine(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_LineScroll)(void* _obj, int columns, int lines)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->LineScroll(columns, lines);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_EnsureCaretVisible)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->EnsureCaretVisible();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ReplaceSelection)(void* _obj, wxChar* text)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ReplaceSelection(text);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetReadOnly)(void* _obj, bool readOnly)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetReadOnly(readOnly);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_CanPaste)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->CanPaste();
#else
  return NULL;
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_CanUndo)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->CanUndo();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_EmptyUndoBuffer)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->EmptyUndoBuffer();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_Undo)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->Undo();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_Cut)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->Cut();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_Copy)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->Copy();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_Paste)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->Paste();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_Clear)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->Clear();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetText)(void* _obj, wxChar* text)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetText(text);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetTextLength)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetTextLength();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetOvertype)(void* _obj, bool overtype)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetOvertype(overtype);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetOvertype)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetOvertype();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetCaretWidth)(void* _obj, int pixelWidth)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetCaretWidth(pixelWidth);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetCaretWidth)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetCaretWidth();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetTargetStart)(void* _obj, int pos)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetTargetStart(pos);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetTargetStart)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetTargetStart();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetTargetEnd)(void* _obj, int pos)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetTargetEnd(pos);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetTargetEnd)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetTargetEnd();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_ReplaceTarget)(void* _obj, wxChar* text)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->ReplaceTarget(text);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_ReplaceTargetRE)(void* _obj, wxChar* text)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->ReplaceTargetRE(text);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_SearchInTarget)(void* _obj, wxChar* text)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->SearchInTarget(text);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetSearchFlags)(void* _obj, int flags)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetSearchFlags(flags);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetSearchFlags)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetSearchFlags();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CallTipShow)(void* _obj, int pos, wxChar* definition)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CallTipShow(pos, definition);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CallTipCancel)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CallTipCancel();
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_CallTipActive)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->CallTipActive();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_CallTipPosAtStart)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->CallTipPosAtStart();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CallTipSetHighlight)(void* _obj, int start, int end)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CallTipSetHighlight(start, end);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CallTipSetBackground)(void* _obj, int back_r, int back_g, int back_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CallTipSetBackground(wxColour(back_r,back_g,back_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CallTipSetForeground)(void* _obj, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CallTipSetForeground(wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CallTipSetForegroundHighlight)(void* _obj, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CallTipSetForegroundHighlight(wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_VisibleFromDocLine)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->VisibleFromDocLine(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_DocLineFromVisible)(void* _obj, int lineDisplay)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->DocLineFromVisible(lineDisplay);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetFoldLevel)(void* _obj, int line, int level)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetFoldLevel(line, level);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetFoldLevel)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetFoldLevel(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetLastChild)(void* _obj, int line, int level)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLastChild(line, level);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetFoldParent)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetFoldParent(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ShowLines)(void* _obj, int lineStart, int lineEnd)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ShowLines(lineStart, lineEnd);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_HideLines)(void* _obj, int lineStart, int lineEnd)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->HideLines(lineStart, lineEnd);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetLineVisible)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLineVisible(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetFoldExpanded)(void* _obj, int line, bool expanded)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetFoldExpanded(line, expanded);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetFoldExpanded)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetFoldExpanded(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ToggleFold)(void* _obj, int line)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ToggleFold(line);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_EnsureVisible)(void* _obj, int line)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->EnsureVisible(line);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetFoldFlags)(void* _obj, int flags)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetFoldFlags(flags);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_EnsureVisibleEnforcePolicy)(void* _obj, int line)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->EnsureVisibleEnforcePolicy(line);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetTabIndents)(void* _obj, bool tabIndents)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetTabIndents(tabIndents);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetTabIndents)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetTabIndents();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetBackSpaceUnIndents)(void* _obj, bool bsUnIndents)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetBackSpaceUnIndents(bsUnIndents);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetBackSpaceUnIndents)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetBackSpaceUnIndents();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetMouseDwellTime)(void* _obj, int periodMilliseconds)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetMouseDwellTime(periodMilliseconds);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetMouseDwellTime)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetMouseDwellTime();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_WordStartPosition)(void* _obj, int pos, bool onlyWordCharacters)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->WordStartPosition(pos, onlyWordCharacters);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_WordEndPosition)(void* _obj, int pos, bool onlyWordCharacters)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->WordEndPosition(pos, onlyWordCharacters);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetWrapMode)(void* _obj, int mode)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetWrapMode(mode);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetWrapMode)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetWrapMode();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetLayoutCache)(void* _obj, int mode)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetLayoutCache(mode);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetLayoutCache)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLayoutCache();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetScrollWidth)(void* _obj, int pixelWidth)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetScrollWidth(pixelWidth);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetScrollWidth)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetScrollWidth();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_TextWidth)(void* _obj, int style, wxChar* text)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->TextWidth(style, text);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetEndAtLastLine)(void* _obj, bool endAtLastLine)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetEndAtLastLine(endAtLastLine);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetEndAtLastLine)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetEndAtLastLine();
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_TextHeight)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->TextHeight(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetUseVerticalScrollBar)(void* _obj, bool show)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetUseVerticalScrollBar(show);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetUseVerticalScrollBar)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetUseVerticalScrollBar();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AppendText)(void* _obj, wxChar* text)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AppendText(text);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetTwoPhaseDraw)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetTwoPhaseDraw();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetTwoPhaseDraw)(void* _obj, bool twoPhase)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetTwoPhaseDraw(twoPhase);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_TargetFromSelection)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->TargetFromSelection();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_LinesJoin)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->LinesJoin();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_LinesSplit)(void* _obj, int pixelWidth)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->LinesSplit(pixelWidth);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetFoldMarginColour)(void* _obj, bool useSetting, int back_r, int back_g, int back_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetFoldMarginColour(useSetting, wxColour(back_r,back_g,back_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetFoldMarginHiColour)(void* _obj, bool useSetting, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetFoldMarginHiColour(useSetting, wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_LineDuplicate)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->LineDuplicate();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_HomeDisplay)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->HomeDisplay();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_HomeDisplayExtend)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->HomeDisplayExtend();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_LineEndDisplay)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->LineEndDisplay();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_LineEndDisplayExtend)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->LineEndDisplayExtend();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_LineCopy)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->LineCopy();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_MoveCaretInsideView)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->MoveCaretInsideView();
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_LineLength)(void* _obj, int line)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->LineLength(line);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_BraceHighlight)(void* _obj, int pos1, int pos2)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->BraceHighlight(pos1, pos2);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_BraceBadLight)(void* _obj, int pos)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->BraceBadLight(pos);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_BraceMatch)(void* _obj, int pos)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->BraceMatch(pos);
#else
  return NULL;
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetViewEOL)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetViewEOL();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetViewEOL)(void* _obj, bool visible)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetViewEOL(visible);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetDocPointer)(void* _obj, void* docPointer)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetDocPointer(docPointer);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetModEventMask)(void* _obj, int mask)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetModEventMask(mask);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetEdgeColumn)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetEdgeColumn();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetEdgeColumn)(void* _obj, int column)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetEdgeColumn(column);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetEdgeMode)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetEdgeMode();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetEdgeMode)(void* _obj, int mode)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetEdgeMode(mode);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetEdgeColour)(void* _obj, int edgeColour_r, int edgeColour_g, int edgeColour_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetEdgeColour(wxColour(edgeColour_r,edgeColour_g,edgeColour_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SearchAnchor)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SearchAnchor();
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_SearchNext)(void* _obj, int flags, wxChar* text)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->SearchNext(flags, text);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_SearchPrev)(void* _obj, int flags, wxChar* text)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->SearchPrev(flags, text);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_LinesOnScreen)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->LinesOnScreen();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_UsePopUp)(void* _obj, bool allowPopUp)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->UsePopUp(allowPopUp);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_SelectionIsRectangle)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->SelectionIsRectangle();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetZoom)(void* _obj, int zoom)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetZoom(zoom);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetZoom)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetZoom();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_AddRefDocument)(void* _obj, void* docPointer)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->AddRefDocument(docPointer);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ReleaseDocument)(void* _obj, void* docPointer)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ReleaseDocument(docPointer);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetModEventMask)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetModEventMask();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetSTCFocus)(void* _obj, bool focus)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetSTCFocus(focus);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetSTCFocus)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetSTCFocus();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetStatus)(void* _obj, int statusCode)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetStatus(statusCode);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetStatus)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetStatus();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetMouseDownCaptures)(void* _obj, bool captures)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetMouseDownCaptures(captures);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetMouseDownCaptures)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetMouseDownCaptures();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetSTCCursor)(void* _obj, int cursorType)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetSTCCursor(cursorType);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetSTCCursor)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetSTCCursor();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetControlCharSymbol)(void* _obj, int symbol)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetControlCharSymbol(symbol);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetControlCharSymbol)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetControlCharSymbol();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_WordPartLeft)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->WordPartLeft();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_WordPartLeftExtend)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->WordPartLeftExtend();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_WordPartRight)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->WordPartRight();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_WordPartRightExtend)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->WordPartRightExtend();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetVisiblePolicy)(void* _obj, int visiblePolicy, int visibleSlop)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetVisiblePolicy(visiblePolicy, visibleSlop);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_DelLineLeft)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->DelLineLeft();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_DelLineRight)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->DelLineRight();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetXOffset)(void* _obj, int newOffset)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetXOffset(newOffset);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetXOffset)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetXOffset();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ChooseCaretX)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ChooseCaretX();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetXCaretPolicy)(void* _obj, int caretPolicy, int caretSlop)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetXCaretPolicy(caretPolicy, caretSlop);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetYCaretPolicy)(void* _obj, int caretPolicy, int caretSlop)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetYCaretPolicy(caretPolicy, caretSlop);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetPrintWrapMode)(void* _obj, int mode)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetPrintWrapMode(mode);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetPrintWrapMode)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetPrintWrapMode();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetHotspotActiveForeground)(void* _obj, bool useSetting, int fore_r, int fore_g, int fore_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetHotspotActiveForeground(useSetting, wxColour(fore_r,fore_g,fore_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetHotspotActiveBackground)(void* _obj, bool useSetting, int back_r, int back_g, int back_b)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetHotspotActiveBackground(useSetting, wxColour(back_r,back_g,back_b));
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetHotspotActiveUnderline)(void* _obj, bool underline)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetHotspotActiveUnderline(underline);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_PositionBefore)(void* _obj, int pos)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->PositionBefore(pos);
#else
  return NULL;
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_PositionAfter)(void* _obj, int pos)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->PositionAfter(pos);
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CopyRange)(void* _obj, int start, int end)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CopyRange(start, end);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CopyText)(void* _obj, int length, wxChar* text)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CopyText(length, text);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StartRecord)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StartRecord();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StopRecord)(void* _obj)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StopRecord();
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetLexer)(void* _obj, int lexer)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetLexer(lexer);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetLexer)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLexer();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_Colourise)(void* _obj, int start, int end)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->Colourise(start, end);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetProperty)(void* _obj, wxChar* key, wxChar* value)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetProperty(key, value);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetKeyWords)(void* _obj, int keywordSet, wxChar* keyWords)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetKeyWords(keywordSet, keyWords);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetLexerLanguage)(void* _obj, wxChar* language)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetLexerLanguage(language);
#endif
}
EWXWEXPORT(int, wxStyledTextCtrl_GetCurrentLine)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetCurrentLine();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetSpec)(void* _obj, int styleNum, wxChar* spec)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetSpec(styleNum, spec);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetFont)(void* _obj, int styleNum, void* font)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetFont(styleNum, *(wxFont*) font);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_StyleSetFontAttr)(void* _obj, int styleNum, int size, wxChar* faceName, bool bold, bool italic, bool underline)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->StyleSetFontAttr(styleNum, size, faceName, bold, italic, underline);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_CmdKeyExecute)(void* _obj, int cmd)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->CmdKeyExecute(cmd);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetMargins)(void* _obj, int left, int right)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetMargins(left, right);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_GetSelection)(void* _obj, void* startPos, void* endPos)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->GetSelection(*(int**) startPos, *(int**) endPos);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ScrollToLine)(void* _obj, int line)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ScrollToLine(line);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_ScrollToColumn)(void* _obj, int column)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->ScrollToColumn(column);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetVScrollBar)(void* _obj, void* bar)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetVScrollBar(*(wxScrollBar**) bar);
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetHScrollBar)(void* _obj, void* bar)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetHScrollBar(*(wxScrollBar**) bar);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_GetLastKeydownProcessed)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->GetLastKeydownProcessed();
#else
  return NULL;
#endif
}
EWXWEXPORT(void, wxStyledTextCtrl_SetLastKeydownProcessed)(void* _obj, bool val)
{
#ifdef wxUSE_STC
   ((wxStyledTextCtrl*) _obj)->SetLastKeydownProcessed(val);
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_SaveFile)(void* _obj, wxChar* filename)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->SaveFile(filename);
#else
  return NULL;
#endif
}
EWXWEXPORT(bool, wxStyledTextCtrl_LoadFile)(void* _obj, wxChar* filename)
{
#ifdef wxUSE_STC
  return ((wxStyledTextCtrl*) _obj)->LoadFile(filename);
#else
  return NULL;
#endif
}
